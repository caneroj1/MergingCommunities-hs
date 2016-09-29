module Main where

import Lib
import Control.Monad
import System.IO

communitySize uf = size uf . root uf

processQueries :: Int -> UnionFind -> IO ()
processQueries 0 _  = return ()
processQueries q uf = do
  (w:ws) <- words <$> getLine
  case w of
    "Q" -> print (communitySize uf ((\x -> x-1) . read $ head ws)) >> processQueries (q-1) uf
    "M" ->
      let (i:j:_) = map read ws
        in processQueries (q-1) (union uf (i-1) (j-1))

main :: IO ()
main = do
  [n,q] <- map read . words <$> getLine
  processQueries q (empty n)
