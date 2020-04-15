{-# OPTIONS_HADDOCK hide #-}

module Render
  ( renderPicture
  )
where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Foreign.JavaScript
import           Control.Monad
import           Data.List
import           Data.List.Split
import           Picture
import           Color
import           Text

renderPicture :: Picture -> Element -> UI ()
renderPicture picture canvas = do
  canvas # saveDrawState
  canvas # translateMiddle
  canvas # drawPicture picture
  canvas # restoreDrawState
  return ()

drawPicture :: Picture -> Element -> UI ()

drawPicture (Blank) canvas = do
  return ()

drawPicture (Circle radius) canvas = do
  canvas # UI.beginPath
  canvas # UI.arc (0, 0) (radius) (-pi) pi
  canvas # UI.closePath
  canvas # UI.fill
  return ()

drawPicture (Arc startAngle endAngle radius) canvas = do
  canvas # UI.beginPath
  canvas # UI.moveTo (0, 0)
  canvas
    # UI.arc (0, 0) (radius) (startAngle * (pi / 180)) (endAngle * (pi / 180))
  canvas # UI.closePath
  canvas # UI.fill
  return ()

drawPicture (Rectangle width height) canvas = do
  canvas # UI.fillRect (0 - (width / 2), 0 - (height / 2)) width height
  return ()

drawPicture (Stroke color size picture) canvas = do
  canvas # saveDrawState
  canvas # set' UI.strokeStyle (convertColor color)
  canvas # set' UI.lineWidth size
  canvas # drawPicture picture
  canvas # restoreDrawState
  return ()

drawPicture (Text string font fontSize) canvas = do
  canvas # set' UI.textAlign (UI.Center)
  canvas # set' UI.textFont (getCombinedFont font fontSize)
  canvas # UI.fillText string (0, 0)
  return ()

drawPicture (Image (Url url) width height) canvas = do
  img <- UI.img # set UI.src url
  canvas # drawImage img (0 - (width / 2), 0 - (height / 2)) width height
  return ()

drawPicture (Image (File file) width height) canvas = do
  img <- UI.img # set
    UI.src
    ("http://127.0.0.1:8023/static/" ++ file)
  canvas # drawImage img (0 - (width / 2), 0 - (height / 2)) width height
  return ()

drawPicture (Scale x y picture) canvas = do
  canvas # saveDrawState
  canvas # scalePicture (x, y)
  canvas # drawPicture picture
  canvas # restoreDrawState
  return ()

drawPicture (Translate x y picture) canvas = do
  canvas # saveDrawState
  canvas # translatePicture (x, y)
  canvas # drawPicture picture
  canvas # restoreDrawState
  return ()

drawPicture (Pictures (picture : pictures)) canvas = do
  canvas # drawPicture picture
  canvas # drawPicture (Pictures pictures)
  return ()

drawPicture (Line (_ : [])) _ = do
  return ()

drawPicture (Line ([])) _ = do
  return ()

drawPicture (Line ((x, y) : rest)) canvas = do
  canvas # UI.beginPath
  canvas # UI.moveTo (x, y)
  forM_ rest (\(x', y') -> canvas # UI.lineTo (x', y'))
  canvas # UI.stroke
  return ()

drawPicture (Polygon (_ : [])) _ = do
  return ()

drawPicture (Polygon ([])) _ = do
  return ()

drawPicture (Polygon ((x, y) : rest)) canvas = do
  canvas # UI.beginPath
  canvas # UI.moveTo (x, y)
  forM_ rest (\(x', y') -> canvas # UI.lineTo (x', y'))
  canvas # UI.closePath
  canvas # UI.fill
  return ()

drawPicture (Color color picture) canvas = do
  canvas # saveDrawState
  canvas # set' UI.fillStyle (UI.htmlColor $ convertColor color)
  canvas # drawPicture picture
  canvas # restoreDrawState
  return ()

drawPicture _ _ = do
  return ()

scalePicture :: Point -> UI.Canvas -> UI ()
scalePicture (sx, sy) canvas =
  UI.runFunction $ ffi "%1.getContext('2d').scale(%2, %3)" canvas sx sy

saveDrawState :: UI.Canvas -> UI ()
saveDrawState canvas = UI.runFunction $ ffi "%1.getContext('2d').save()" canvas

restoreDrawState :: UI.Canvas -> UI ()
restoreDrawState canvas =
  UI.runFunction $ ffi "%1.getContext('2d').restore()" canvas

resetTransform :: UI.Canvas -> UI ()
resetTransform canvas = UI.runFunction
  $ ffi "%1.getContext('2d').setTransform(1, 0, 0, 1, 0, 0)" canvas

translatePicture :: Point -> UI.Canvas -> UI ()
translatePicture (tx, ty) canvas =
  UI.runFunction $ ffi "%1.getContext('2d').translate(%2, %3)" canvas tx ty

translateMiddle :: UI.Canvas -> UI ()
translateMiddle canvas = UI.runFunction
  $ ffi "%1.getContext('2d').translate(%1.width/2, %1.height/2)" canvas

drawImage :: UI.Element -> Vector -> Double -> Double -> UI.Canvas -> UI ()
drawImage image (x, y) width height canvas = UI.runFunction $ ffi
  "%1.getContext('2d').drawImage(%2,%3,%4,%5,%6)"
  canvas
  image
  x
  y
  width
  height

getMimeType :: String -> String
getMimeType fileName = case (last (splitOn "." fileName)) of
  "apng"  -> "image/apng"
  "bmp"   -> "image/bmp"
  "gif"   -> "image/gif"
  "ico"   -> "image/x-icon"
  "cur"   -> "image/x-icon"
  "jpg"   -> "image/jpeg"
  "jpeg"  -> "image/jpeg"
  "jfif"  -> "image/jpeg"
  "pjpeg" -> "image/jpeg"
  "pjp"   -> "image/jpeg"
  "png"   -> "image/png"
  "svg"   -> "image/svg+xml"
  "tif"   -> "image/tiff"
  "tiff"  -> "image/tiff"
  "webp"  -> "image/webp"
  _       -> "image"
