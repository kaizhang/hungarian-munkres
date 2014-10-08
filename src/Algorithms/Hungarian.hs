{-# LANGUAGE ForeignFunctionInterface #-}

module Algorithms.Hungarian 
    ( hungarian
    , hungarianScore
    , unsafeHungarian
    , unsafeHungarianScore
    ) where

import Data.List
import Foreign
import Foreign.C
import System.IO.Unsafe

foreign import ccall "hungarian"
    c_hungarian :: Ptr CDouble -> CInt -> CInt -> Ptr CSize -> Ptr CSize -> IO Double

-- | solve the LSAP by hungarian algorithm, return assignment and score.
hungarian :: [Double]               -- ^ row majored flat matrix
          -> Int                    -- ^ number of rows
          -> Int                    -- ^ number of columns
          -> ([(Int, Int)], Double)
hungarian costMatrix rows cols
    | length costMatrix /= rows * cols = error "Algorithms.Hungarian.hungarian: incorrect size"
    | otherwise = unsafeHungarian costMatrix rows cols
{-# INLINE hungarian #-}

-- | solve the LSAP by hungarian algorithm, return score only
hungarianScore :: [Double] -> Int -> Int -> Double
hungarianScore costMatrix rows cols
    | length costMatrix /= rows * cols = error "Algorithms.Hungarian.hungarian: incorrect size"
    | otherwise = unsafePerformIO $ do
        withArray (map realToFrac costMatrix) $ \input -> do
            fmap realToFrac $ c_hungarian input (fromIntegral rows)
                                          (fromIntegral cols) nullPtr nullPtr
{-# INLINE hungarianScore #-}

-- | doesn't check if the input is a valid matrix
unsafeHungarian :: [Double]               -- ^ row majored flat matrix
                -> Int                    -- ^ number of rows
                -> Int                    -- ^ number of columns
                -> ([(Int, Int)], Double)
unsafeHungarian costMatrix rows cols = unsafePerformIO $ do
    withArray (map realToFrac costMatrix) $ \input -> 
        allocaArray n $ \from -> allocaArray n $ \to -> do
            cost <- c_hungarian input (fromIntegral rows) (fromIntegral cols)
                                from to
            froms <- peekArray n from
            tos <- peekArray n to
            return (zipWith f froms tos, realToFrac cost)
  where
    f x y = (fromIntegral x, fromIntegral y)
    n = min rows cols
{-# INLINE unsafeHungarian #-}

-- | solve the LSAP by hungarian algorithm, return score only
unsafeHungarianScore :: [Double] -> Int -> Int -> Double
unsafeHungarianScore costMatrix rows cols = unsafePerformIO $ do
    withArray (map realToFrac costMatrix) $ \input -> do
        fmap realToFrac $ c_hungarian input (fromIntegral rows)
                                      (fromIntegral cols) nullPtr nullPtr
{-# INLINE unsafeHungarianScore #-}
