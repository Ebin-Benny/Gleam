{-# LANGUAGE DeriveDataTypeable #-}

module Picture
  ( Picture(..)
  , Point
  , Vector
  , Path
  , Location(..)

        -- * Aliases for Picture constructors
  , blank
  , polygon
  , line
  , circle
  , arc
  , text
  , color
  , translate
  , rotate
  , scale
  , pictures

        -- * Compound shapes
  , lineLoop
  , sectorWire
  , rectanglePath
  , rectangleWire
  , rectangleSolid
  , rectangleUpperPath
  , rectangleUpperWire
  , rectangleUpperSolid
  )
where

import           Data.Monoid
import           Data.Semigroup
import           Data.Foldable

-- | A point on the x-y plane.
type Point = (Double, Double)

-- | A vector can be treated as a point, and vis-versa.
type Vector = Point

-- | A path through the x-y plane.
type Path = [Point]

data Location = File String | Url String
  deriving (Show, Eq)

-- | A 2D picture
data Picture
        -- | A blank picture, with nothing in it.
        = Blank
        -- | A convex polygon filled with a solid color.
        | Polygon       Path
        -- | A line along an arbitrary path.
        | Line          Path
        -- | A circle with the given radius.
        | Circle        Double
        -- | A circular arc drawn counter-clockwise between two angles
        --  (in degrees) at the given radius.
        | Arc           Double Double Double
        -- | Some text to draw with a vector font.
        | Text          String String String
        -- | Image to draw from local file or url.
        | Image         Location Double Double
        -- Color ------------------------------------------
        -- | A picture drawn with this color.
        | Color         String Picture
        -- Transforms -------------------------------------
        -- | A picture translated by the given x and y coordinates.
        | Translate     Double Double Picture
        -- | A picture rotated clockwise by the given angle (in degrees).
        | Rotate        Double Picture
        -- | A picture scaled by the given x and y factors.
        | Scale         Double Double Picture
        -- More Pictures ----------------------------------
        -- | A picture consisting of several others.
        | Pictures      [Picture]
        deriving (Show, Eq)


-- Instances ------------------------------------------------------------------
instance Monoid Picture where
  mempty = Blank
  mappend a b = Pictures [a, b]
  mconcat = Pictures

instance Semigroup Picture where
  a <> b = Pictures [a, b]
  sconcat = Pictures . toList
  stimes  = stimesIdempotent

-- | A blank picture, with nothing in it.
blank :: Picture
blank = Blank

-- | A convex polygon filled with a solid color.
polygon :: Path -> Picture
polygon = Polygon

-- | A line along an arbitrary path.
line :: Path -> Picture
line = Line

-- | A circle with the given radius.
circle :: Double -> Picture
circle = Circle

-- | A circular arc drawn counter-clockwise between two angles (in degrees)
--   at the given radius.
arc :: Double -> Double -> Double -> Picture
arc = Arc

-- | Some text to draw with a vector font.
text :: String -> String -> String -> Picture
text = Text

-- | A picture drawn with this color.
color :: String -> Picture -> Picture
color = Color

-- | A picture translated by the given x and y coordinates.
translate :: Double -> Double -> Picture -> Picture
translate = Translate

-- | A picture rotated clockwise by the given angle (in degrees).
rotate :: Double -> Picture -> Picture
rotate = Rotate

-- | A picture scaled by the given x and y factors.
scale :: Double -> Double -> Picture -> Picture
scale = Scale

-- | A picture consisting of several others.
pictures :: [Picture] -> Picture
pictures = Pictures


-- Other Shapes ---------------------------------------------------------------
-- | A closed loop along a path.
lineLoop :: Path -> Picture
lineLoop []       = Line []
lineLoop (x : xs) = Line ((x : xs) ++ [x])


-- | A wireframe sector of a circle.
--   An arc is draw counter-clockwise from the first to the second angle at
--   the given radius. Lines are drawn from the origin to the ends of the arc.
---
--   NOTE: We take the absolute value of the radius incase it's negative.
--   It would also make sense to draw the sector flipped around the
--   origin, but I think taking the absolute value will be less surprising
--   for the user.
--
sectorWire :: Double -> Double -> Double -> Picture
sectorWire a1 a2 r_ =
  let r = abs r_
  in  Pictures
        [ Arc a1 a2 r
        , Line [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))]
        , Line [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))]
        ]


-- Rectangles -----------------------------------------------------------------
-- NOTE: Only the first of these rectangle functions has haddocks on the
--       arguments to reduce the amount of noise in the extracted docs.

-- | A path representing a rectangle centered about the origin
rectanglePath
  :: Double        -- ^ width of rectangle
  -> Double        -- ^ height of rectangle
  -> Path
rectanglePath sizeX sizeY =
  let sx = sizeX / 2
      sy = sizeY / 2
  in  [(-sx, -sy), (-sx, sy), (sx, sy), (sx, -sy)]


-- | A wireframe rectangle centered about the origin.
rectangleWire :: Double -> Double -> Picture
rectangleWire sizeX sizeY = lineLoop $ rectanglePath sizeX sizeY


-- | A wireframe rectangle in the y > 0 half of the x-y plane.
rectangleUpperWire :: Double -> Double -> Picture
rectangleUpperWire sizeX sizeY = lineLoop $ rectangleUpperPath sizeX sizeY


-- | A path representing a rectangle in the y > 0 half of the x-y plane.
rectangleUpperPath :: Double -> Double -> Path
rectangleUpperPath sizeX sy =
  let sx = sizeX / 2 in [(-sx, 0), (-sx, sy), (sx, sy), (sx, 0)]


-- | A solid rectangle centered about the origin.
rectangleSolid :: Double -> Double -> Picture
rectangleSolid sizeX sizeY = Polygon $ rectanglePath sizeX sizeY


-- | A solid rectangle in the y > 0 half of the x-y plane.
rectangleUpperSolid :: Double -> Double -> Picture
rectangleUpperSolid sizeX sizeY = Polygon $ rectangleUpperPath sizeX sizeY

-- | Convert degrees to radians
degToRad :: Double -> Double
degToRad d = d * pi / 180
{-# INLINE degToRad #-}


-- | Convert radians to degrees
radToDeg :: Double -> Double
radToDeg r = r * 180 / pi
{-# INLINE radToDeg #-}


-- | Normalize an angle to be between 0 and 2*pi radians
normalizeAngle :: Double -> Double
normalizeAngle f = f - 2 * pi * floor' (f / (2 * pi))
 where
  floor' :: Double -> Double
  floor' x = fromIntegral (floor x :: Int)
