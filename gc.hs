{-# LANGUAGE MagicHash #-}
module Main where

import Control.Monad
import Data.Word (Word64)
import qualified Data.ByteString.Char8 as BS8
import Control.DeepSeq
import qualified Data.Text.Array as A
import GHC.Prim
import GHC.Base (Int(..))
import Control.Exception (evaluate)

type BA = A.Array

makeBAs n = replicate n (A.run $ A.new 100)
instance NFData A.Array where
  rnf x = I# i `seq` ()
    where i = indexInt8Array# (A.aBA x) 0#

bs = force $ BS8.pack $ show [1..100]

makeBSs n = replicate n (BS8.reverse bs)

doIt ::
  (Num a, Traversable t, NFData b, NFData (t b)) =>
  (a -> IO (t b)) -> IO ()
doIt makeItems = do
  forM_ [1..(50 :: Int)] $ \i -> do
    let n = 100000
    bas <- makeItems n
    fbas <- mapM (evaluate . force) bas
    putStrLn $ fbas `deepseq` show i

main :: IO ()
main = do
--  doIt (return . makeBAs)
  doIt (return . makeBSs)
  return ()
