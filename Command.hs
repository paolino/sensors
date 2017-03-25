-- | controlling commands (todo: implement discrete commands, Lef, Rig, Halt)

module Command (command) where


import Control.Monad (forever, join)
import Text.Read (readMaybe)
import Data.List (splitAt)
import Data.List.PointedList (focus, previous, next)
import Control.Lens (view,set,over,_1,_2)

import Control.Concurrent.STM (newTChanIO, readTChan, atomically
	, STM, writeTChan,TVar,readTVar,orElse,modifyTVar)
import Control.Concurrent (forkIO, killThread)
import Data.Machine.Mealy (runMealy)

import Library (stepMoore)
import Renderer (Input(Control))
import Filter (filterParam,inertia,record)
import Midi (midiInputControl,midiInputNote)
import State (State, renderers, halting, filters)

data Command = Lef | Rig | Halt | Plugin Int Float  | Inertia Int Int | Repeating Int Bool deriving Read

aquireMidi :: IO (STM Command, IO ())
aquireMidi = do
	chanC <- newTChanIO
	tc <- forkIO $ midiInputControl "sensors controlin" (atomically . writeTChan chanC)
        
	chanN <- newTChanIO
	tn <- forkIO $ midiInputNote "sensors notein" (atomically . writeTChan chanN)
        
        let readC = do
                (_,i,v) <- readTChan chanC
                return $ case i of 
                        21 -> Inertia 0 (v + 1)
                        22 -> Inertia 1 (v + 1)
                        23 -> Inertia 2 (v + 1)
                        24 -> Inertia 3 (v + 1)
                        25 -> Inertia 4 (v + 1)
                        26 -> Inertia 5 (v + 1)
                        27 -> Inertia 6 (v + 1)
                        28 -> Inertia 7 (v + 1)
                        41 -> Inertia 8 (v + 1)
                        _  ->  Plugin i (fromIntegral v / 128)
        let readN = do
                (_,n,v,b) <- readTChan chanN
                return $ Repeating (n `mod` 9) b
                        
	return (readC `orElse` readN, killThread tc >> killThread tn)

aquireKbd ::  IO (STM Command, IO ())
aquireKbd = do
	chan <- newTChanIO
	t <- forkIO . forever $ do
		putStr "command:"
		l <- readMaybe `fmap` getLine
		case l of 
			Nothing -> return ()
			Just c -> atomically $ writeTChan chan c
	return (readTChan chan, killThread t)
	
	
parse :: Command -> State a -> State a
parse Lef = nextZipper previous
parse Rig = nextZipper next
parse Halt = set halting True
parse (Inertia i n) = over filters $ \ws ->
        let     (bf,x:af) = splitAt i ws
                x' = set (filterParam . inertia) n x
        in bf ++ (x':af)
parse (Repeating i b) = over filters $ \ws ->
        let     (bf,x:af) = splitAt i ws
                x' = set (filterParam . record) b x
        in bf ++ (x':af)

parse (Plugin i x) = over (renderers . focus) (stepMoore $ Control i x)

-- unfailable movement
nextZipper f s = case f $ view renderers s of 
		Nothing -> s
		Just p -> set renderers p s


-- | a state modifier aquiring commands from midi inputs and kbd, must be forked
command :: TVar (State a) -> IO ()
command state = do 
	(waitm,endm) <- aquireMidi
	(waitk,endk) <- aquireKbd
	let 	loop =  join . atomically $ do 
				com <- waitm `orElse` waitk
				modifyTVar state $ parse com
				s <- readTVar state
				if view halting s then return $ endm >> endk else return loop 
	loop

