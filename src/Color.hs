module Color
  ( Color(..)
  , convertColor
  )
where

data Color
        = White
        | Black
        | Transparent
        | Red
        | Green
        | Blue
        | Yellow
        | Cyan
        | Magenta
        | Rose
        | Violet
        | Azure
        | Aquamarine
        | Chartreuse
        | Orange
        -- | A hex color string.
        | RGBA String
        deriving (Show, Eq)


-- | Converts a color to a html color string.
convertColor :: Color -> String
convertColor color = case (color) of
  White         -> "white"
  Black         -> "black"
  Transparent   -> "#00000000"
  Red           -> "red"
  Green         -> "green"
  Blue          -> "blue"
  Yellow        -> "yellow"
  Cyan          -> "cyan"
  Magenta       -> "magenta"
  Rose          -> "rose"
  Violet        -> "violet"
  Azure         -> "azure"
  Chartreuse    -> "chartreuse"
  Orange        -> "orange"
  (RGBA string) -> string
