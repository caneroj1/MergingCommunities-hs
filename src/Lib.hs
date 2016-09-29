module Lib
    ( empty
    , union
    , find
    , size
    , root
    , UnionFind
    ) where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data UnionFind = UnionFind {
  idxs   :: Seq Int
, sizeAt :: Seq Int
}

empty :: Int -> UnionFind
empty n = UnionFind (Seq.fromList [0..(n-1)]) (Seq.replicate n 1)

union :: UnionFind -> Int -> Int -> UnionFind
union uf@UnionFind{idxs = idxs, sizeAt = sz} i j
  | ri == rj        = uf
  | riSize < rjSize = UnionFind (Seq.update ri rj idxs) (Seq.update rj newSz sz)
  | otherwise       = UnionFind (Seq.update rj ri idxs) (Seq.update ri newSz sz)
  where
    (ri, rj) = (root uf i, root uf j)
    riSize   = Seq.index sz ri
    rjSize   = Seq.index sz rj
    newSz    = riSize + rjSize

find :: UnionFind -> Int -> Int -> Bool
find uf i j = root uf i == root uf j

size :: UnionFind -> Int -> Int
size UnionFind{sizeAt = sizes} = Seq.index sizes

root :: UnionFind -> Int -> Int
root uf@UnionFind{idxs = idxs} i
  | parentOf i == i = i
  | otherwise       = root uf $ parentOf i
  where parentOf = Seq.index idxs
