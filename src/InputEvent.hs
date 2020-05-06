module InputEvent
  ( InputEvent(..)
  , Key(..)
  , KeyState(..)
  , SpecialKey(..)
  )
where

import           Picture

-- | An input event.
data InputEvent
        -- | A key or mouse button event
        = EventKey Key KeyState
        -- | A mouse motion event
        | EventMouse Point Point
        deriving (Eq, Show)

-- | A key.
data Key
        -- | A key that can be represented by a character
        = Char          Char
        -- | A special key.
        | SpecialKey    SpecialKey
        -- | A mouse button. 
        | Mouse         Point
        deriving (Show, Eq, Ord)

-- | State of the key event.
data KeyState
        = Down
        | Up
        deriving (Show, Eq, Ord)

-- | Special keys
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
