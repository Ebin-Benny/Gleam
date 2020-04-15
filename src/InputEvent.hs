module InputEvent
  ( InputEvent(..)
  , Key(..)
  , KeyState(..)
  , SpecialKey(..)
  )
where

import           Picture

data InputEvent
        = EventKey Key KeyState
        | EventMotion Point Point
        deriving (Eq, Show)

data Key
        = Char          Char
        | SpecialKey    SpecialKey
        | Mouse         Point
        deriving (Show, Eq, Ord)

data KeyState
        = Down
        | Up
        deriving (Show, Eq, Ord)

data SpecialKey
        = KeyUnknown
        | KeySpace
        | KeyEsc
        | KeyUp
        | KeyDown
        | KeyLeft
        | KeyRight
        | KeyTab
        | KeyEnter
        | KeyBackspace
        | KeyShift
        | KeyCtrl
        | KeyAlt
        | KeyCaps
        deriving (Show, Eq, Ord)
