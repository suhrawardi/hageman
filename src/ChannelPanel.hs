{-# LANGUAGE Arrows #-}

module ChannelPanel (
    channelPanel
  ) where

import Buttons
import Data.Maybe (mapMaybe, isJust)
import Debugger
import Euterpea
import HSoM
import FRP.UISF
import RandPanel
import Scales
import System.Random


adjustM :: MidiMessage -> MidiMessage
adjustM (ANote c k _ _)      = Std (ControlChange c 2 k)
adjustM (Std (NoteOn c k _)) = Std (ControlChange c 2 k)
adjustM a                    = a


adjustMode :: UISF (Int, Maybe [MidiMessage]) (Maybe [MidiMessage])
adjustMode = proc (mode, mo) ->
    if mode == 0
      then
        returnA -< (fmap (map adjustM) mo)
      else
        returnA -< mo


asNote :: Int -> Maybe Int -> Int -> Double -> Maybe [MidiMessage]
asNote c Nothing  v d = Nothing
asNote c (Just n) v d = Just [ANote c n 127 d]


channelPanel :: UISF (Maybe OutputDeviceID, Double) (Maybe [MidiMessage])
channelPanel = topDown $ setSize (560, 670) $ title "Channel" $ proc (mo, f) -> do
    (channel, isPlaying, isLearning) <- buttonsPanel -< ()
    mode <- leftRight $ title "Mode" $ radio ["drone", "rhythm"] 0 -< ()
    d <- title "Dur" $ withDisplay (hiSlider 1 (1, 16) 1) -< ()

    (note, vel, dur, tick) <- (| leftRight ( do
      tuning <- topDown $ setSize (270, 290) $ title "Scale" $ radio otherScales 0 -< ()
      tick' <- timer -< 1/f
      f' <- randPanel -< (f, tick')
      let dur = (1/f') * fromIntegral d
          vel = 127
      tick <- timer -< dur
      note <- randNote -< (scale tuning, 3, tick)
      returnA -< (note, vel, dur, tick) ) |)

    if isLearning
      then do
        returnA -< fmap (const [ANote channel 36 100 01]) tick
      else do
        if isPlaying
          then do
            moM <- adjustMode -< (mode, asNote channel note vel dur)
            returnA -< moM
          else
            returnA -< Nothing


sGen :: StdGen
sGen = mkStdGen 42
