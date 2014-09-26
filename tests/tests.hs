import Algorithms.Hungarian
import Data.Algorithm.Munkres
import Data.Array.Unboxed
import Test.Tasty
import Test.Tasty.QuickCheck
import System.Random

main :: IO ()
main = defaultMain $ testGroup "Hungarian-Munkres"
    [ testProperty "Hungarian" (\x -> uncurry (~=) $ t_hungarian x)
    ]

t_hungarian :: Int -> (Double, Double)
t_hungarian g = (hungarianScore xs rows cols, solution)
  where
    solution = snd $ hungarianMethodDouble $ listArray ((1,1), (rows, cols)) xs
    (xs, rows, cols) = randMat (mkStdGen g)

randMat :: StdGen -> ([Double], Int, Int)
randMat g = (take (rows * cols) $ randoms g, rows, cols)
  where
    [rows, cols] = take 2 $ randomRs (100, 150) g

(~=) :: Double -> Double -> Bool
(~=) a b = abs (a - b) <= 1.0e-12
