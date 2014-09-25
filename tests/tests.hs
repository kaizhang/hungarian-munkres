import Algorithms.Hungarian
import Data.Algorithm.Munkres
import Data.Array.Unboxed
import Numeric.IEEE
import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "Hungarian-Munkres"
    [ testProperty "Hungarian" t_hungarian
    ]

t_hungarian :: [[Double]] -> Bool
t_hungarian xs | null xs = True
               | otherwise = hungarian xs' rows cols ~= solution
  where
    solution = snd $ hungarianMethodDouble $ listArray ((1,1), (rows, cols)) xs'
    xs' = concat xs
    rows = length xs
    cols = length . head $ xs


(~=) :: Double -> Double -> Bool
(~=) a b = abs (a - b) < epsilon * max (abs a) (abs b)
