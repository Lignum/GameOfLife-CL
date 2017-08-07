{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Main where

import Compute
import Graphics
import Simulation

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Semigroup ((<>))
import Data.Maybe

import SDL (($=))
import qualified SDL.Video.Renderer as SDL
import qualified SDL.Event as SDL
import qualified SDL.Vect as SDL

run :: MonadIO m => Simulation -> m ()
run sim@(Simulation gc@GraphicsContext{..} cc@ComputeContext{..} _ _ _ _ _) = do
  evts <- SDL.pollEvents
  let rdr  = graphicsRenderer
      win  = graphicsWindow
      quit = or . flip fmap evts $ \(SDL.eventPayload -> e) -> runReader (simHandleEvent e) sim
  unless quit $ do
    SDL.rendererDrawColor rdr $= SDL.V4 0 127 255 255
    SDL.clear rdr
    runReaderT simRun sim
    SDL.present rdr
    run sim

main :: IO ()
main = withGraphicsContext $ \gc -> do
  err <- withComputeContext $ simulation gc >=> run
  case err of
    Just err -> putStrLn $ "Failed to create OpenCL context: " <> err
    Nothing  -> pure ()