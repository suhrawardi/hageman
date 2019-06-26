{-# LANGUAGE Arrows #-}

module MainUI (
    runMainUI
  ) where

import ChannelPanel
import Euterpea
import FRP.UISF
import HSoM


runMainUI = runMUI (styling "Composer" (2040, 680)) mainUI


mainUI :: UISF () ()
mainUI = leftRight $ proc _ -> do
  (mo, f) <- (| topDown ( do
    mo <- selectOutput -< ()
    f <- title "Dur" $ withDisplay (hiSlider 1 (1, 16) 1) -< ()
    returnA -< (mo, f) ) |)

  out1 <- channelPanel -< (mo, fromIntegral f)
  out2 <- channelPanel -< (mo, fromIntegral f)
  out3 <- channelPanel -< (mo, fromIntegral f)
  midiOut -< (mo, mappend (mappend out1 out2) out3)


styling :: String -> (Int, Int) -> UIParams
styling title (h, w) = defaultMUIParams {uiTitle = title, uiSize = (h, w)}
