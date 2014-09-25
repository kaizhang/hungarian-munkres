{-# LANGUAGE ForeignFunctionInterface #-}

module Algorithms.Hungarian 
    ( hungarian
    ) where

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import System.IO.Unsafe

foreign import ccall "hungarian"
    c_hungarian :: Ptr CDouble -> Ptr CInt -> CInt -> CInt -> IO CInt

hungarian :: [[Int]] -> [Int]
hungarian costMatrix = unsafePerformIO $ do 
    input <- newArray . concat . (map . map) fromIntegral $ costMatrix
    output <- mallocArray (nrows * ncols)
    e <- c_hungarian input output (fromIntegral nrows) (fromIntegral ncols)
    results <- peekArray (nrows * ncols) output
    return $ map fromIntegral results
  where
    nrows = length costMatrix
    ncols = length . head $ costMatrix
