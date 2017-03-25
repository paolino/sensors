
import Prelude hiding ((.), id)
import Control.Category ((.), id)
import Data.List.PointedList (fromList,focus)
import Data.Copointed (copoint)
import Control.Lens (view,over,_1,_2,set)
import Control.Monad (forever, join)
import Data.Maybe (catMaybes)
import Control.Arrow (arr)
-- IO & concurrency
import Control.Concurrent.STM (atomically, readTVar, newTVarIO, writeTVar, modifyTVar, newTChanIO, writeTChan,STM, readTChan, retry, TVar)
import Control.Concurrent (forkIO, killThread, threadDelay)
import System.Environment (getArgs)

-- graphics
-- import Graphics.Gloss (makeColor, Display (InWindow), Picture(Blank))
-- import Graphics.Gloss.Interface.IO.Animate (animateIO)

import Graphics.UI.GLUT
-- local
import Command (command)
import Sensors (sensors)
import State (renderers, State (State),pausing) 
import Library (stepMoore)
import Filter (sensorFilter, SensorFilter (..))
import Renderer (Renderer)
import Midi (midiOut)

-- | Plugins
import Renderers.Tri1

-- initial renderers, from imported modules
-- plugins :: [Renderer [Maybe Midiv]]
plugins = [Renderers.Tri1.renderering] 

-- program initial state
state0 =  State 
	(maybe (error "no programs?") id $ fromList plugins) -- the zipper
	(replicate 9 $ sensorFilter)
	False  -- runningx
        False

{- 
		
summers :: STM [Midiv] -> IO ()
summers f = do
        chan <- newTChanIO
        forkIO $ midiOut  "sensors effect" (atomically $ readTChan chan) 
        forever $ do 
                threadDelay 40000
                -- atomically f >>= print 
                atomically $ f >>= mapM_ (\(n,v) -> writeTChan chan (1,n + 9,v))
  -}              
                

main = do
	state <- newTVarIO state0
        countframes <- newTVarIO (0,0)      
	_ <- forkIO $ command state
	_ <- forkIO $ sensors state $ modifyTVar countframes (over _2 (+1))
        {-
        _ <- forkIO . summers . join $ (\x -> if False then retry else return x) `fmap`
                (catMaybes .snd . copoint . view (renderers . focus)) `fmap` 
                readTVar state
        _ <- forkIO . forever $ do
                threadDelay 200000
                t <- atomically $ (snd . copoint . view (renderers . focus)) `fmap` readTVar state
                print t
        -}
        _ <- forkIO . forever $ do
                threadDelay 1000000
                (video,dataf) <- atomically (readTVar countframes) 
                print (video,dataf)
                atomically $ do 
                        modifyTVar state $ set pausing (video <= 0) 
                        writeTVar countframes (0,0)

        (_progName, _args) <- getArgsAndInitialize
        initialDisplayMode $= [DoubleBuffered]
        _window <- createWindow "sensors"
        displayCallback $= display state (modifyTVar countframes (over _1 (+1)))
        blend $= Enabled 
        blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
        mainLoop



display :: TVar (State ()) -> STM () -> IO ()
display state counter  = do 
  clear [ColorBuffer]
  join . atomically $  (fst . copoint . view (renderers . focus)) `fmap` readTVar state
  swapBuffers
  atomically counter
  postRedisplay Nothing
