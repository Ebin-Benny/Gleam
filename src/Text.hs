module Text
  ( Font(..)
  , FontSize(..)
  , getCombinedFont
  )
where

import           Data.List

-- | A font.
data Font
  = Arial
  | Verdana
  | TimesNewRoman
  | CourierNew
  | Serif
  | SansSerif
  -- | A html font family
  | Font String deriving (Show, Eq)

-- | A font size, given in points.
data FontSize = Size Int deriving (Show, Eq)

-- | Converts a `Font` to a html font family.
convertFont :: Font -> String
convertFont font = case (font) of
  Arial         -> "Arial"
  Verdana       -> "Verdana"
  TimesNewRoman -> "Times New Roman"
  CourierNew    -> "Courier New"
  Serif         -> "serif"
  SansSerif     -> "sans-serif"
  Font family   -> family

-- | Converts a `FontSize` to a html font size.
convertFontSize :: FontSize -> String
convertFontSize (Size size) = (show size) ++ "pt"

-- | Combines a `Font` and `FontSize` to return a html string representing them.
getCombinedFont :: Font -> FontSize -> String
getCombinedFont font fontSize =
  intercalate " " [(convertFontSize fontSize), (convertFont font)]




