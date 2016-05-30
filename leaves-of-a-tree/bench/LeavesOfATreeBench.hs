import           Criterion.Main
import           Data.BinTree

tree0 :: BinTree Integer
tree0 = mkTree [0 .. 1000]

tree1 :: BinTree Integer
tree1 = mkTree [0 .. 1000000]

treeE0 :: BinTreeE Integer
treeE0 = mkTreeE [0 .. 1000]

treeE1 :: BinTreeE Integer
treeE1 = mkTreeE [0 .. 1000000]

main :: IO ()
main = defaultMain [
  bgroup "leaves" [
      bench "catamorphism with standard list/10^3 nodes" (nf leavesCata treeE0)
    , bench "catamorphism with standard list/10^6 nodes" (nf leavesCata treeE1)
    , bench "lazy state monad/10^3 nodes" (nf leavesS tree0)
    , bench "lazy state monad/10^6 nodes" (nf leavesS tree1)
    , bench "recursion with standard list/10^3 nodes" (nf leaves tree0)
    , bench "recursion with standard list/10^6 nodes" (nf leaves tree1)
    , bench "recursion with difference list/10^3 nodes" (nf leaves' tree0)
    , bench "recursion with difference list/10^6 nodes" (nf leaves' tree1)
    ]
  ]
