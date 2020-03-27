module Utility
  ( setAttribute
  )
where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core

setAttribute :: String -> String -> UI.Canvas -> UI ()
setAttribute key value canvas =
  UI.runFunction $ ffi "%1.setAttribute(%2, %3)" canvas key value
