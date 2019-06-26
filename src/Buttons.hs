{-# LANGUAGE Arrows #-}

module Buttons (
    buttonsPanel,
  ) where

import Data.Maybe (isJust)
import FRP.UISF


buttonsPanel :: UISF () (Int, Bool, Bool)
buttonsPanel = title "Start/Stop" $ topDown $ proc _ -> do
    channel <- leftRight $ title "Channel" $ withDisplay (hiSlider 1 (1, 12) 1) -< ()
    (isPlaying, isLearning) <- (| leftRight ( do
      isPlaying <- checkbox "Play" False -< ()
      isLearning <- checkbox "Learn" False -< ()
      returnA -< (isPlaying, isLearning) ) |)

    returnA -< (channel, isPlaying, isLearning)
