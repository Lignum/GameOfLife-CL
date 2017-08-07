{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Graphics(GraphicsContext(..)
              , withGraphicsContext) where

import qualified SDL.Init as SDL
import qualified SDL.Video as SDL
import qualified SDL.Video.Renderer as SDL

data GraphicsContext
  = GraphicsContext { graphicsWindow   :: SDL.Window
                    , graphicsRenderer :: SDL.Renderer }

withGraphicsContext :: (GraphicsContext -> IO ()) -> IO ()
withGraphicsContext f = do
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  win <- SDL.createWindow "Conway's CL Life" SDL.defaultWindow
  rdr <- SDL.createRenderer win (-1) $ SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedRenderer }
  let gc = GraphicsContext win rdr
     in f gc >> destroyContext gc >> pure ()

destroyContext :: GraphicsContext -> IO ()
destroyContext GraphicsContext{..} = do
  SDL.destroyRenderer graphicsRenderer
  SDL.destroyWindow graphicsWindow
  SDL.quit