{-# LANGUAGE ForeignFunctionInterface #-}

module Algorithms.Hungarian 
    ( hungarian
    , hungarianScore
    ) where

import Data.List
import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array
import System.IO.Unsafe

foreign import ccall "hungarian"
    c_hungarian :: Ptr CDouble -> Ptr CInt -> CInt -> CInt -> IO Double

-- | solve the LSAP by hungarian algorithm, return assignment and score
hungarian :: [Double]               -- ^ row majored flat matrix
          -> Int                    -- ^ number of rows
          -> Int                    -- ^ number of columns
          -> ([(Int, Int)], Double)
hungarian costMatrix nrows ncols = unsafePerformIO $ do
    withArray (map realToFrac costMatrix) $ \input -> 
        allocaArray (nrows * ncols) $ \output -> do
            cost <- c_hungarian input output (fromIntegral nrows) (fromIntegral ncols)
            results <- peekArray (nrows * ncols) output
            return (getAssign results, realToFrac cost)
  where
    getAssign :: [CInt] -> [(Int, Int)]
    getAssign = snd . foldl' step (0,[])
    step (i,assign) x | x == 0 = (i+1, assign)
                      | otherwise = (i+1, (i `div` ncols, i `mod` ncols) : assign)
{-# INLINE hungarian #-}

-- | solve the LSAP by hungarian algorithm, return score only
hungarianScore :: [Double] -> Int -> Int -> Double
hungarianScore costMatrix nrows = snd . hungarian costMatrix nrows
{-# INLINE hungarianScore #-}
