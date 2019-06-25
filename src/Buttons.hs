{-# LANGUAGE Arrows #-}

module Buttons (
    buttonsPanel,
  ) where

import Data.Maybe (isJust)
import FRP.UISF


buttonsPanel :: UISF () (Int, Bool, Bool)
buttonsPanel = title "Start/Stop" $ leftRight $ proc _ -> do
    channel <-title "Channel" $ withDisplay (hiSlider 1 (1, 12) 1) -< ()
    isPlaying <- checkbox "Play" False -< ()
    isLearning <- checkbox "Learn" False -< ()
    returnA -< (channel, isPlaying, isLearning)
