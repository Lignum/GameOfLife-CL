{-# LANGUAGE FlexibleContexts, RecordWildCards #-}
module Compute(ComputeContext(..)
             , withComputeContext) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except
import Control.Parallel.OpenCL.Query
import Control.Parallel.OpenCL.Context

import Data.Maybe

data ComputeContext
  = ComputeContext { computePlatform  :: CLPlatformID
                   , computeDevice    :: CLDeviceID
                   , computeCLContext :: CLContext }
  deriving (Show, Eq, Ord)

firstElement :: MonadError String m => String -> [a] -> m a
firstElement err []  = throwError err
firstElement _ (x:_) = pure x

firstPlatform :: (MonadIO m, MonadError String m) => m CLPlatformID
firstPlatform = liftIO clGetPlatformIDs >>= firstElement "Couldn't find an eligible platform!"

firstDevice :: (MonadIO m, MonadError String m) => CLPlatformID -> m CLDeviceID
firstDevice p = liftIO (clGetDeviceIDs p CL_DEVICE_TYPE_ALL) >>= firstElement "Couldn't find an eligible device!"

withComputeContext :: (ComputeContext -> IO ()) -> IO (Maybe String)
withComputeContext f = do
  c <- runExceptT $ do
    plt <- firstPlatform 
    dev <- firstDevice plt
    ctx <- liftIO $ clCreateContext [CL_CONTEXT_PLATFORM plt] [dev] (const $ pure ())
    pure $ ComputeContext plt dev ctx
  case c of
    Left err -> pure $ Just err
    Right c  -> f c >> destroyContext c >> pure Nothing

destroyContext :: ComputeContext -> IO ()
destroyContext ComputeContext{..} = do
  clReleaseContext computeCLContext
  pure ()