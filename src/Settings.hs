{-# LANGUAGE ExistentialQuantification #-}

module Settings
  ( Simulation(..)
  , GleamConfig(..)
  , defaultGleamConfig
  )
where

import Picture
import InputEvent

data Simulation = forall model . Simulation {
  -- | Config for the canvas.
  simConfig :: GleamConfig,
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

data GleamConfig = GleamConfig {
  -- | Width of the canvas.
  width :: Int,
  -- | Height of the canvas.
  height :: Int
}

-- | The default config for Gleam
defaultGleamConfig :: GleamConfig
defaultGleamConfig = GleamConfig 400 400