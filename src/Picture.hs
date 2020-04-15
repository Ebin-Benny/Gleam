{-# LANGUAGE DeriveDataTypeable #-}

module Picture
  ( Picture(..)
  , Point
  , Vector
  , Path
  , Source(..)
        -- * Compound shapes
  , lineLoop
  , sectorWire
  , rectanglePath
  , rectangleWire
  , rectangleSolid
  )
where

import           Data.Monoid
import           Data.Semigroup
import           Data.Foldable
import           Color
import           Text

-- | A point on the x-y plane.
type Point = (Double, Double)

-- | A vector can be treated as a point, and vis-versa.
type Vector = Point

-- | A path through the x-y plane.
type Path = [Point]

-- | An image location
data Source =
  -- | Path to an image inside ./images. 
  File String
  -- | An image url.
  | Url String
  deriving (Show, Eq)

-- | A 2D picture
data Picture
        -- | A blank picture, with nothing in it.
        = Blank
        -- | A line along an arbitrary path.
        | Line          Path
        -- | A polygon filled with a solid color.
        | Polygon       Path
        -- | A circle with the given radius.
        | Circle        Double
        -- | A circular arc drawn counter-clockwise between two angles
        --  (in degrees) at the given radius.
        | Arc           Double Double Double
        -- | A rectangle drawn with given width and height.
        | Rectangle     Double Double
        -- | Image to draw from a certain with given width and height.
        | Image         Source Double Double
        -- | Some text to draw with a vector font.
        | Text          String Font FontSize
        -- | A picture drawn with this color.
        | Color     Color Picture
        -- | A picture drawn with this stroke, given a color and size.
        | Stroke        Color Double Picture
        -- | A picture translated by the given x and y coordinates.
        | Translate     Double Double Picture
        -- | A picture scaled by the given x and y factors.
        | Scale         Double Double Picture
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

-- Other Shapes ---------------------------------------------------------------
-- | A closed loop along a path.
lineLoop :: Path -> Picture
lineLoop []       = Line []
lineLoop (x : xs) = Line ((x : xs) ++ [x])


-- | A wireframe sector of a circle.
--   An arc is draw counter-clockwise from the first to the second angle at
--   the given radius.
sectorWire :: Double -> Double -> Double -> Picture
sectorWire a1 a2 r_ =
  let r = abs r_
  in  Pictures
        [ Arc a1 a2 r
        , Line [(0, 0), (r * cos (degToRad a1), r * sin (degToRad a1))]
        , Line [(0, 0), (r * cos (degToRad a2), r * sin (degToRad a2))]
        ]


-- Rectangles -----------------------------------------------------------------

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


-- | A solid rectangle centered about the origin.
rectangleSolid
  :: Double         -- ^ width of rectangle
  -> Double         -- ^ height of rectangle
  -> Picture
rectangleSolid sizeX sizeY = Polygon $ rectanglePath sizeX sizeY

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
