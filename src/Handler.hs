{-# OPTIONS_HADDOCK hide #-}

module Handler
  ( handleEventsMultiple
  , handleEvents
  )
where

import qualified Graphics.UI.Threepenny        as UI
import           Graphics.UI.Threepenny.Core
import           Data.Char
import           Data.IORef
import           InputEvent
import           Picture
import           Settings

-- | Handles events for multiple canvases.
handleEventsMultiple
  :: GleamConfig                    -- ^ Canvas size.
  -> IORef model                    -- ^ Current state of the simulation.
  -> IORef (Double, Double)         -- ^ Current mouse position.
  -> IORef Bool                     -- ^ Whether the current simulation is paused.
  -> (InputEvent -> model -> model) -- ^ Function to handle input events.
  -> UI.Element                     -- ^ The canvas element.
  -> UI ()
handleEventsMultiple gleamconfig currentState currentMousePos currentPause handler canvas
  = do
    on UI.keydown canvas $ \c -> do
      pause <- liftIO $ readIORef currentPause
      case (pause) of
        False -> do
          current <- liftIO $ readIORef currentState
          let updatedState = handler (convertKeyCode c Down) current
          liftIO $ writeIORef currentState updatedState
        True -> return ()

    on UI.keyup canvas $ \c -> do
      pause <- liftIO $ readIORef currentPause
      case (pause) of
        False -> do
          current <- liftIO $ readIORef currentState
          let updatedState = handler (convertKeyCode c Up) current
          liftIO $ writeIORef currentState updatedState
        True -> return ()

    on UI.mouseup canvas $ \pos -> do
      pause <- liftIO $ readIORef currentPause
      case (pause) of
        False -> do
          current <- liftIO $ readIORef currentState
          let updatedState = handler
                (convertMouse (convertMousePos gleamconfig pos) Up)
                current
          liftIO $ writeIORef currentState updatedState
        True -> return ()

    on UI.mousedown canvas $ \pos -> do
      pause <- liftIO $ readIORef currentPause
      case (pause) of
        False -> do
          current <- liftIO $ readIORef currentState
          let updatedState = handler
                (convertMouse (convertMousePos gleamconfig pos) Down)
                current
          liftIO $ writeIORef currentState updatedState
        True -> return ()

    on UI.mousemove canvas $ \pos -> do
      pause <- liftIO $ readIORef currentPause
      case (pause) of
        False -> do
          current  <- liftIO $ readIORef currentState
          mousePos <- liftIO $ readIORef currentMousePos
          let updatedState = handler
                (convertMouseMove mousePos (convertMousePos gleamconfig pos))
                current
          liftIO $ writeIORef currentState updatedState
          liftIO $ writeIORef currentMousePos $ convertMousePos gleamconfig pos
        True -> return ()

    return ()

-- | Handles events for a single canvas.
handleEvents
  :: GleamConfig                    -- ^ Canvas size.
  -> IORef model                    -- ^ Current state of the simulation.
  -> IORef (Double, Double)         -- ^ Current mouse position.
  -> (InputEvent -> model -> model) -- ^ Function to handle input events.
  -> UI.Element                     -- ^ The canvas element.
  -> UI ()
handleEvents gleamconfig currentState currentMousePos handler canvas = do

  on UI.keydown canvas $ \c -> do
    current <- liftIO $ readIORef currentState
    let updatedState = handler (convertKeyCode c Down) current
    liftIO $ writeIORef currentState updatedState

  on UI.keyup canvas $ \c -> do
    current <- liftIO $ readIORef currentState
    let updatedState = handler (convertKeyCode c Up) current
    liftIO $ writeIORef currentState updatedState

  on UI.mouseup canvas $ \pos -> do
    current <- liftIO $ readIORef currentState
    let updatedState =
          handler (convertMouse (convertMousePos gleamconfig pos) Up) current
    liftIO $ writeIORef currentState updatedState

  on UI.mousedown canvas $ \pos -> do
    current <- liftIO $ readIORef currentState
    let updatedState =
          handler (convertMouse (convertMousePos gleamconfig pos) Down) current
    liftIO $ writeIORef currentState updatedState

  on UI.mousemove canvas $ \pos -> do
    current  <- liftIO $ readIORef currentState
    mousePos <- liftIO $ readIORef currentMousePos
    let updatedState = handler
          (convertMouseMove mousePos (convertMousePos gleamconfig pos))
          current
    liftIO $ writeIORef currentState updatedState
    liftIO $ writeIORef currentMousePos $ convertMousePos gleamconfig pos

  return ()

convertMousePos :: GleamConfig -> (Int, Int) -> Point
convertMousePos gleamconfig (x, y) =
  ( ((fromIntegral x) - (fromIntegral (width gleamconfig) / 2))
  , ((fromIntegral y) - (fromIntegral (height gleamconfig) / 2))
  )

convertMouse :: Point -> KeyState -> InputEvent
convertMouse pos state = (EventKey (Mouse pos) state)

convertMouseMove :: Point -> Point -> InputEvent
convertMouseMove (x, y) (nx, ny) = (EventMouse ((x - nx), (y - ny)) (nx, ny))

convertKeyCode :: UI.KeyCode -> KeyState -> InputEvent
convertKeyCode code state
  | charCodes code = (EventKey (Char (keyCodeToChar code)) state)
  | code == 8      = (EventKey (SpecialKey KeyBackspace) state)
  | code == 9      = (EventKey (SpecialKey KeyTab) state)
  | code == 13     = (EventKey (SpecialKey KeyEnter) state)
  | code == 16     = (EventKey (SpecialKey KeyShift) state)
  | code == 17     = (EventKey (SpecialKey KeyCtrl) state)
  | code == 18     = (EventKey (SpecialKey KeyAlt) state)
  | code == 20     = (EventKey (SpecialKey KeyCaps) state)
  | code == 27     = (EventKey (SpecialKey KeyEsc) state)
  | code == 37     = (EventKey (SpecialKey KeyLeft) state)
  | code == 38     = (EventKey (SpecialKey KeyUp) state)
  | code == 39     = (EventKey (SpecialKey KeyRight) state)
  | code == 40     = (EventKey (SpecialKey KeyDown) state)
  | otherwise      = (EventKey (SpecialKey KeyUnknown) state)

keyCodeToChar :: UI.KeyCode -> Char
keyCodeToChar code | (code >= 65 && code <= 90) = chr $ (ord 'z') - (90 - code)
                   | (code >= 48 && code <= 57) = chr $ (ord '9') - (57 - code)
                   | (code == 186)              = ';'
                   | (code == 187)              = '='
                   | (code == 188)              = ','
                   | (code == 189)              = '-'
                   | (code == 190)              = '.'
                   | (code == 191)              = '/'
                   | (code == 192)              = '`'
                   | (code == 219)              = '['
                   | (code == 220)              = '\\'
                   | (code == 221)              = ']'
                   | (code == 222)              = '\''
                   | otherwise                  = '?'

charCodes :: UI.KeyCode -> Bool
charCodes code | (code >= 65 && code <= 90)   = True
               | (code >= 48 && code <= 61)   = True
               | (code >= 48 && code <= 57)   = True
               | (code >= 186 && code <= 192) = True
               | (code >= 219 && code <= 222) = True
               | otherwise                    = False