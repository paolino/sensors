{-# LANGUAGE ViewPatterns,ScopedTypeVariables #-}
module Midi where

import Prelude hiding (catch)
import Sound.ALSA.Sequencer.Client (setName, getId)
import Sound.ALSA.Sequencer.Port (withSimple, caps, capWrite, capSubsWrite, typeMidiGeneric, capSubsRead,capRead)
import Sound.ALSA.Sequencer.Event (input, body, Data (CtrlEv, NoteEv,QueueEv), QueueEv (QueueClock), NoteEv(NoteOn, NoteOff) ,CtrlEv(Controller,SongPos), Note(..)
        , Ctrl(..), Channel(..), Velocity (..), Parameter(..), Value(..),Pitch (..), Duration(..),forConnection,outputDirect)
import Sound.ALSA.Sequencer (InputMode, OutputMode, T, withDefault, BlockMode(Block))
import Sound.ALSA.Sequencer.Address (T(Cons))
import Sound.ALSA.Sequencer.Connect (toSubscribers)
-- import Sound.ALSA.Sequencer.Queue 
import Sound.ALSA.Exception (catch, T)
import Control.Monad (forever)
-- import Control.Exception

midiInputControl  :: String               -- ^ client name
        -> ((Int,Int,Int)  -> IO ()) -- ^ event channel
        -> IO ()
midiInputControl name incha = withDefault Block $ \(h :: Sound.ALSA.Sequencer.T InputMode)  -> do
        setName h name
        
        withSimple h "in control" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \_ -> forever $ do
                ev <-  input h
                case body ev of
                     CtrlEv Controller (Ctrl 
                                        (Channel (fromIntegral -> cha)) 
                                        (Parameter (fromIntegral -> par)) 
                                        (Value (fromIntegral -> val))
                                        ) 
                       -> incha (cha,par,val)
                     _ -> return ()

midiInputSync name incha = withDefault Block $ \(h :: Sound.ALSA.Sequencer.T InputMode)  -> do
        setName h name
        
        withSimple h "in sync" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \_ -> forever $ do
                ev <-  input h
                case body ev of
                     QueueEv QueueClock _ -> incha 0
                     CtrlEv SongPos (Ctrl {ctrlChannel = Channel {unChannel = 0}, 
                                ctrlParam = Parameter {unParameter = 0}, 
                                ctrlValue = Value {unValue = 160}}
                                ) -> incha 1 
                     _ -> return ()

                    
midiInputNote  :: String               -- ^ client name
        -> ((Int,Int,Int,Bool)  -> IO ()) -- ^ event channel
        -> IO ()
midiInputNote name incha = withDefault Block $ \(h :: Sound.ALSA.Sequencer.T InputMode)  -> do
        setName h name
        
        withSimple h "in note" (caps [capWrite, capSubsWrite]) typeMidiGeneric $ \_ -> forever $ do
                ev <-  input h
                case body ev of
                     NoteEv NoteOn (Note 
                                        (Channel (fromIntegral -> cha)) 
                                        (Pitch (fromIntegral -> par)) 
                                        (Velocity (fromIntegral -> val))
                                        _ _
                                        ) 
                       -> incha (cha,par,val,True)
                     NoteEv NoteOff (Note 
                                        (Channel (fromIntegral -> cha)) 
                                        (Pitch (fromIntegral -> par)) 
                                        (Velocity (fromIntegral -> val))
                                        _ _
                                        ) 
                       -> incha (cha,par,val,False)
                     _ -> return ()

	
-- | Loop-broadcast control midi message on a specific channel
midiOut :: String  -- ^ client name
        -> IO (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiOut name  ech = withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) name
        withSimple h "out control" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (cha,par,val) <- ech
                c <- getId h
                let ev =  forConnection (toSubscribers (Cons c p)) $ 
                                CtrlEv Controller (Ctrl 
                                        (Channel $ fromIntegral cha) 
                                        (Parameter $ fromIntegral par) 
                                        (Value $ fromIntegral val)
                                        )
                catch  (outputDirect h ev >> return ())(\(e :: Sound.ALSA.Exception.T) -> return ())
                return ()

-- | Loop-broadcast control midi message on a specific channel
midiOutNote :: String  -- ^ client name
        -> IO (Int,Int,Int)  -- ^ event channel
        -> IO ()
midiOutNote name  ech = withDefault Block $ \h -> do
        setName (h :: Sound.ALSA.Sequencer.T OutputMode) name
        withSimple h "out control" (caps [capRead, capSubsRead]) typeMidiGeneric $ \p -> forever $ do
                (cha,note,val) <- ech
                c <- getId h
                let ev =  forConnection (toSubscribers (Cons c p)) $ 
                                NoteEv NoteOn (Note 
                                        (Channel $ fromIntegral cha) 
                                        (Pitch $ fromIntegral note) 
                                        (Velocity $ fromIntegral val)
					(Velocity 0) (Duration 0)
                                        )
                catch  (outputDirect h ev >> return ())(\(e :: Sound.ALSA.Exception.T) -> return ())
                return ()

