import Algorithms.Hungarian

main = do r <- hungarian [[1,2,3],[2,0,0],[3,3,12]]
          print r
    
