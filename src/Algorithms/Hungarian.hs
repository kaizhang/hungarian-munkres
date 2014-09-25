{-# LANGUAGE ForeignFunctionInterface #-}

module Algorithms.Hungarian 
    ( hungarian
    ) where

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import System.IO.Unsafe

foreign import ccall "hungarian"
    c_hungarian :: Ptr CDouble -> Ptr CInt -> CInt -> CInt -> IO Double

hungarian :: [Double] -> Int -> Int -> Double
hungarian costMatrix nrows ncols = unsafePerformIO $ do
    withArray (map realToFrac costMatrix) $ \input -> 
        allocaArray (nrows * ncols) $ \output -> do
            cost <- c_hungarian input output (fromIntegral nrows) (fromIntegral ncols)
--    results <- peekArray (nrows * ncols) output
            return $ realToFrac cost
