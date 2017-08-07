{-# LANGUAGE FlexibleContexts, RecordWildCards, ViewPatterns, ScopedTypeVariables #-}
module Simulation(Simulation(..)
                , simulation
                , simHandleEvent
                , simRun) where

import Compute
import Graphics

import Control.Monad.State.Strict
import Control.Monad.IO.Class
import Control.Parallel.OpenCL.Memory
import Control.Parallel.OpenCL.CommandQueue
import Control.Parallel.OpenCL.Program

import Data.Word
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as VS

import Foreign.C.Types (CInt)
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr

import SDL (($=))
import qualified SDL.Event as SDL
import qualified SDL.Video.Renderer as SDL
import qualified SDL.Vect as SDL

import System.Random
import System.IO

data Simulation
  = Simulation { simGraphicsContext :: GraphicsContext
               , simComputeContext  :: ComputeContext
               , simBoardWidth      :: Int
               , simBoardHeight     :: Int
               , simCommandQueue    :: CLCommandQueue
               , simCellBuffer      :: CLMem
               , simKernel          :: CLKernel }

rect :: Integral i => i -> i -> i -> i -> SDL.Rectangle CInt
rect x y w h = SDL.Rectangle (SDL.P $ SDL.V2 (f x) (f y)) $ SDL.V2 (f w) (f h) where
  f = fromIntegral

randomCells :: Int -> Int -> IO (VS.Vector Bool)
randomCells w h = VS.generateM (w * h) $ const randomIO

simDefaultBoardWidth = 250
simDefaultBoardHeight = 250

simBoardCellWidth = 2
simBoardCellHeight = 2

simulation :: GraphicsContext -> ComputeContext -> IO Simulation
simulation gc cc@ComputeContext{..} = do
  let (w, h) = (simDefaultBoardWidth, simDefaultBoardHeight)
  cq  <- clCreateCommandQueue computeCLContext computeDevice []
  cls <- randomCells w h
  let size = sizeOf True * w * h
  cbf <- VS.unsafeWith cls $ \buf -> clCreateBuffer computeCLContext [CL_MEM_COPY_HOST_PTR] (size, castPtr buf)
  src <- readFile "kernels/gol.cl"
  cpr <- clCreateProgramWithSource computeCLContext src
  clBuildProgram cpr [computeDevice] "-cl-mad-enable"
  ckr <- clCreateKernel cpr "step_board"
  clReleaseProgram cpr
  clUnloadCompiler
  pure $ Simulation gc cc w h cq cbf ckr

simHandleEvent :: MonadState Simulation m => SDL.EventPayload -> m Bool
simHandleEvent SDL.QuitEvent = pure True 
simHandleEvent _ = pure False

simWithCells :: (MonadIO m, MonadState Simulation m) => (VS.Vector Bool -> IO a) -> m a
simWithCells f = do
  bw  <- gets simBoardWidth
  bh  <- gets simBoardHeight
  cq  <- gets simCommandQueue
  cbf <- gets simCellBuffer
  let size = sizeOf True * bw * bh
  x <- liftIO $ do
    (evt, ptr) <- clEnqueueMapBuffer cq cbf True [CL_MAP_READ, CL_MAP_WRITE] 0 size []
    fptr <- newForeignPtr_ ptr
    let x' = f . VS.unsafeFromForeignPtr0 (castForeignPtr fptr :: ForeignPtr Bool) $ bw * bh
    clEnqueueUnmapMemObject cq cbf ptr [evt]
    x'

  pure x

simDrawCells :: (MonadIO m, MonadState Simulation m) => Int -> Int -> m ()
simDrawCells w h = do
  rdr <- graphicsRenderer <$> gets simGraphicsContext
  bw  <- gets simBoardWidth
  bh  <- gets simBoardHeight
  simWithCells $ \cls ->
    flip V.imapM_ cls $ \i c -> let (x, y) = (i `mod` bw, i `div` bw)
                                  in do if c then SDL.rendererDrawColor rdr $= SDL.V4 0 0 0 255
                                             else SDL.rendererDrawColor rdr $= SDL.V4 255 255 255 255
                                        SDL.fillRect rdr . Just $ rect (x * w) (y * h) w h

simStep :: (MonadIO m, MonadState Simulation m) => m ()
simStep = do
  ckr <- gets simKernel
  cbf <- gets simCellBuffer
  cq  <- gets simCommandQueue
  bw  <- gets simBoardWidth
  bh  <- gets simBoardHeight
  liftIO $ do
    clSetKernelArgSto ckr 0 cbf
    clSetKernelArgSto ckr 1 (fromIntegral bw :: Word32)
    clSetKernelArgSto ckr 2 (fromIntegral bh :: Word32)
    let size = bw * bh
    evt <- clEnqueueNDRangeKernel cq ckr [bw, bh] [] []
    clEnqueueWaitForEvents cq [evt]

simRun :: (MonadIO m, MonadState Simulation m) => m ()
simRun = do
  simStep
  simDrawCells simBoardCellWidth simBoardCellHeight

simDestroy :: (MonadIO m, MonadState Simulation m) => m ()
simDestroy = do
  cbf <- gets simCellBuffer
  ckr <- gets simKernel
  liftIO $ do
    clReleaseMemObject cbf
    clReleaseKernel ckr
  pure ()