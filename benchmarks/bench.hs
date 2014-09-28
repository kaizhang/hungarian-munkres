import Algorithms.Hungarian
import Criterion.Main
import Data.Algorithm.Munkres
import Data.Array.Unboxed
import System.Random

sample :: [Double]
sample = take 4000 $ randomRs (-100, 100) (mkStdGen 4)

sample' :: [Double]
sample' = take 40000 $ randomRs (-1000, 1000) (mkStdGen 24)

sampleArray :: UArray (Int, Int) Double
sampleArray = listArray ((1,1), (40,50)) sample

sampleArray' :: UArray (Int, Int) Double
sampleArray' = listArray ((1,1), (200,200)) sample'

main :: IO ()
main = defaultMain 
    [ bgroup "Hungarian Algorithm Benchmarks"
        [ bench "C version (40 * 50)" $ nf (\x -> hungarian x 40 50) sample
        , bench "Pure Haskell (40 * 50)" $ nf hungarianMethodDouble sampleArray
        , bench "C version (200 * 200)" $ nf (\x -> hungarian x 200 200) sample'
        , bench "Pure Haskell (200 * 200)" $ nf hungarianMethodDouble sampleArray'
        ]
    ]
