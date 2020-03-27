module Animate
  ( animateMultiple
  , animate
  )
where

import           Picture
import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.Timer
import           Data.IORef
import           Render

animateMultiple
  :: IORef model          -- | Current state of the simulation.
  -> IORef Bool           -- | Whether the current simulation is paused.
  -> (model -> model)     -- | Function to update the state of the simulation.
  -> (model -> Picture)   -- | Function to generate a picture from a model.
  -> UI.Element           -- | The canvas element.
  -> UI Timer
animateMultiple currentState currentPause update draw canvas = do
  t <- timer
  on (\canvas -> (tick t)) canvas $ \_ -> do
    pause <- liftIO $ readIORef currentPause
    case (pause) of
      False -> do
        current <- liftIO $ readIORef currentState
        let updatedState = update current
        liftIO $ writeIORef currentState updatedState
        renderPicture (draw updatedState) canvas
      True -> do
        current <- liftIO $ readIORef currentState
        renderPicture (draw current) canvas
  return t # set interval 16 # set running True

animate
  :: IORef model        -- | Current state of the simulation.
  -> (model -> model)   -- | Function to update the state of the simulation.
  -> (model -> Picture) -- | Function to generate a picture from a model.
  -> UI.Element         -- | The canvas element.
  -> UI Timer
animate currentState update draw canvas = do
  t <- timer
  on (\canvas -> (tick t)) canvas $ \_ -> do
    current <- liftIO $ readIORef currentState
    let updatedState = update current
    liftIO $ writeIORef currentState updatedState
    renderPicture (draw updatedState) canvas
  return t # set interval 16 # set running True
