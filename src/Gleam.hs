{-# LANGUAGE ExistentialQuantification #-}

module Gleam
  ( play
  , playMultiple
  , Simulation(..)
  , module Picture
  , module InputEvent
  )
where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Control.Monad
import           Data.IORef
import           Control.Monad.Trans            ( liftIO )
import           Picture
import           Animate
import           InputEvent
import           Handler
import           Utility

canvasSize :: Int
canvasSize = 400

data Simulation = forall model . Simulation {
  -- | Initial model for a simulation.
  simInitialModel :: model,
  -- | Function to generate a picture from a model.
  simDraw :: (model -> Picture),
  -- | Function to update the state of the simulation.
  simUpdate :: (model -> model),
  -- | Function to handle input events.
  simHandler :: (InputEvent -> model -> model),
  -- | Title of the simulation.
  simTitle :: String
}

config = defaultConfig { jsStatic = Just "./images"}

-- | Run a simulation in a window. You decide how the model is represented, how to convert the model to a picture and how to update the model. This function does the rest.
play
  :: model                          -- | Initial model for the simulation.
  -> (model -> Picture)             -- | Function to generate a picture from a model.
  -> (model -> model)               -- | Function to update the state of the simulation.
  -> (InputEvent -> model -> model) -- | Function to handle input events.
  -> IO ()
play initialModel draw update handler =
  startGUI config $ setup initialModel draw update handler

-- | Run multiple simulations in a window. You decide how each model is represented, how to convert each model to a picture and how to update the model. This function does the rest.
playMultiple :: [Simulation] -> IO ()
playMultiple simulations = startGUI config $ setupMultiple simulations

setup
  :: model                          -- | Initial model for the simulation.
  -> (model -> Picture)             -- | Function to generate a picture from a model.
  -> (model -> model)               -- | Function to update the state of the simulation.
  -> (InputEvent -> model -> model) -- | Function to handle input events.
  -> Window
  -> UI ()
setup initialModel draw update handler window = do
  return window # set title "ThreePennyGloss"

  canvas <- UI.canvas # set UI.width canvasSize # set UI.height canvasSize # set
    UI.style
    [("background", "#bbb")]

  canvas # setAttribute "tabindex" "1"

  getBody window #+ [element canvas]

  currentState    <- liftIO $ newIORef initialModel

  currentMousePos <- liftIO $ newIORef (0.0, 0.0)

  handleEvents currentState currentMousePos (handler) canvas

  animate currentState (update) (draw) canvas

  return ()

setupMultiple :: [Simulation] -> Window -> UI ()
setupMultiple simulations window = do
  return window # set title "ThreePennyGloss"
  simulate simulations window
  return ()

simulate :: [Simulation] -> Window -> UI ()
simulate ([]) _ = do
  return ()
simulate ((Simulation simInitialModel simDraw simUpdate simHandler simTitle) : simulations) window
  = do
    return ()
    canvas <-
      UI.canvas # set UI.width canvasSize # set UI.height canvasSize # set
        UI.style
        [("background", "#bbb")]

    canvas # setAttribute "tabindex" "1"

    text          <- UI.p # set UI.text simTitle
    playButton    <- UI.button # set UI.class_ "play"
    restartButton <- UI.button # set UI.class_ "restart"

    buttonDiv     <- UI.div # set UI.children [playButton, restartButton] # set
      UI.class_
      "buttons"

    getBody window #+ [element text, element buttonDiv, element canvas]

    currentState    <- liftIO $ newIORef simInitialModel

    currentMousePos <- liftIO $ newIORef (0.0, 0.0)

    currentPause    <- liftIO $ newIORef False

    on UI.click playButton $ \_ -> do
      pause <- liftIO $ readIORef currentPause
      liftIO $ writeIORef currentPause (not pause)

    on UI.click restartButton $ \_ -> do
      liftIO $ writeIORef currentState simInitialModel

    handleEventsMultiple currentState
                         currentMousePos
                         currentPause
                         (simHandler)
                         canvas

    animateMultiple currentState currentPause (simUpdate) (simDraw) canvas

    simulate simulations window
