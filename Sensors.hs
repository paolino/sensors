{-# LANGUAGE ViewPatterns #-}
module Sensors (sensors) where

import Prelude hiding ((.), id)

import System.IO (hGetLine, hClose)
import System.Hardware.Serialport (hOpenSerial, defaultSerialSettings, commSpeed, CommSpeed(CS115200))

import Control.Monad (join, forM_, when,void , forever)
import Control.Lens (view,set,over)
import Data.Copointed (copoint)
import Control.Category ((.), id)
import Control.Arrow ((&&&))
import Control.Concurrent.STM
import Control.Concurrent

import Data.List.PointedList (focus)

import Data.Maybe (fromJust)

import Text.Read (readMaybe)

import State (renderers, halting, State, filters, pausing) 
import Library (stepMoore)
import Filter (stepControlFilter, filterParam, reset)
import Renderer (Input(Sensor))
import Midi (midiOut, midiOutNote,midiInputNote)

import Control.Exception

import System.Environment
ignore :: IOException -> IO String
ignore _ = return ""


data Source = Source {
    params :: STM [Float]
    }
    
acquire :: IO (STM (Int,Int,Int,Bool), (Int,Maybe Int) -> STM (), STM [Float], IO ())
acquire = do
    args <- getArgs
    -- port <- head `fmap` getArgs -- /dev/ttyUSB0 must be given (arduino usb port)
    let port = "/dev/ttyUSB" ++ args !! 0
    sync <- newTChanIO
    sensor <- newTVarIO $ replicate 9 0.5
    chanm <- newTChanIO 
    chann <- newTChanIO     
    let     send (n, Nothing) = return ()
            send (n, Just v) =  writeTChan chanm (0,n + read (args !! 1) * 9,v)
    forkIO $ midiOut "sensors" (atomically $ readTChan chanm)
    -- forkIO $ forever (atomically $ readTChan chanm)
    forkIO $ midiInputNote "sensors sync" (atomically . writeTChan sync)
    
    -- sensor signal
    handl <- hOpenSerial port defaultSerialSettings{commSpeed=CS115200}
    t <- forkIO . forever $ do
            line <- readMaybe `fmap` handle ignore (hGetLine handl)
            maybe (print line) (atomically . writeTVar sensor) line
            
    
    
    return (readTChan sync,send, readTVar sensor, killThread t >> hClose handl)

-- | a state modifier closing on the sensors filters.
-- The modification is about stepping the renderer feeding in the input from the sensors. The renderer is a moore machine and so it exposes the actual picture 


sensors :: TVar (State a)-> STM () -> IO () 
sensors state counter =  do 
    (sync, send, sens ,end) <- acquire
    let     loop  =  do      
                        threadDelay 5000
                        rs <- atomically sens -- sample sensor values
                        res <- atomically $  (Just `fmap`) sync `orElse` return Nothing
                        --res <- atomically $  return Nothing
                        join . atomically $ do 
                                p <- view pausing `fmap` readTVar state  -- check if the Picture is rendering
                                if not p then do
                                        counter -- count on receiving
                                        ws <- view filters `fmap` readTVar state
                                        let ws' = case res of
                                                Nothing -> map (set (filterParam . reset) False) ws
                                                Just (_,_,_,True) -> map (set (filterParam . reset) True) ws
                                                _ -> ws
                                        -- step the filters ws in ws' by feeding raw rs adn obtaining midi (mqs) and normalized (-1,1) values
                                        let     (unzip -> (qs,mqs),ws'') = unzip $ zipWith stepControlFilter ws' rs 
                                        -- update the renderer at focus by stepping its moore machine
                                        modifyTVar state . over (renderers . focus) . stepMoore $ Sensor qs
                                        modifyTVar state . set filters $ ws''
                                        forM_ (zip [0..] mqs) send
                                        s <- view halting `fmap` readTVar state
                                        if s then return end else return loop 
                                   else return loop
    loop 




