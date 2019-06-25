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


channelPanel :: UISF (Maybe OutputDeviceID, Double) (Maybe [MidiMessage])
channelPanel = topDown $ setSize (560, 670) $ title "Channel" $ proc (mo, f) -> do
    (channel, isPlaying, isLearning) <- buttonsPanel -< ()
    mode <- leftRight $ title "Mode" $ radio ["drone", "rhythm"] 0 -< ()
    tick <- timer -< 1/f

    dur <- randPanel -< (f, tick)

    (scale) <- (| leftRight ( do
      scale <- topDown $ setSize (250, 290) $ title "Scale" $ radio otherScales 0 -< ()
      returnA -< (scale) ) |)

    if isLearning
      then do
        returnA -< fmap (const [ANote channel 36 100 01]) tick
      else do
        if isPlaying
          then do
            returnA -< fmap (const [ANote channel 36 100 01]) tick
            -- moM' -< adjustMode -< (mode, moM)
            -- returnA -< moM
          else
            returnA -< Nothing


octaves :: [(String, Octave)]
octaves = [("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5),
           ("6", 6), ("7", 7), ("8", 8), ("9", 9), ("10", 10)]


sGen :: StdGen
sGen = mkStdGen 42
