{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.DeepSeq (rnf)
import Data.Foldable (Foldable (fold))
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Builder
import GHC.IO (evaluate)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)
import Typist.Logged (
  Logged (..),
  Unquoted (..),
  fmt, (@=),
 )

toStrictText = Lazy.toStrict . Builder.toLazyText

{-# INLINE get #-}
get :: (?list :: [a], ?f :: (a -> b)) => Int -> b
get i = ?f $ ?list !! i

{-# NOINLINE concatTest #-}
concatTest :: (Logged a) => [a] -> Strict.Text
concatTest list =
  toStrictText
    let ?list = list
        ?f = toLog
     in "1"
          <> get 0
          <> "22"
          <> get 1
          <> "333"
          <> get 2
          <> "4444"
          <> get 3
          <> "55555"
          <> get 4
          <> "666666"
          <> get 5
          <> "7777777"
          <> get 6
          <> "88888888"
          <> get 7
          <> "999999999"
          <> get 8
          <> "1010101010"
          <> get 9
          <> "11111111111"
          <> get 10
          <> "121212121212"
          <> get 11
          <> "1313131313131"
          <> get 12

{-# NOINLINE formatTest #-}
formatTest :: (Logged a) => [a] -> Strict.Text
formatTest list =
  toStrictText
    let ?list = list
        ?f = id
     in fmt
          @"1#{i0}22#{i1}333#{i2}4444#{i3}55555#{i4}666666#{i5}7777777#{i6}88888888#{i7}\
           \999999999#{i8}1010101010#{i9}11111111111#{i10}121212121212#{i11}1313131313131#{i12}" $
          (#i0 @= get 0) .
          (#i1 @= get 1) .
          (#i2 @= get 2) .
          (#i3 @= get 3) .
          (#i4 @= get 4) . 
          (#i5 @= get 5) . 
          (#i6 @= get 6) .
          (#i7 @= get 7) .
          (#i8 @= get 8) . 
          (#i9 @= get 9) .
          (#i10 @= get 10) .
          (#i11 @= get 11) .
          (#i12 @= get 12) 

{-# NOINLINE jobs #-}
jobs :: Int -> ([a] -> Strict.Text) -> [[a] -> Strict.Text]
jobs = replicate

main :: IO ()
main = do
  let list = [1 .. 20] :: [Int]
  _ <- evaluate (rnf list)
  let benches =
        [1, 10, 100, 1000, 10000] >>= \n ->
          [ bgroup
              ("Formatting " <> show n <> " strings")
              [ bench "concat" $ nf (\l -> foldMap ($ l) $ jobs n concatTest) list
              , bench "format" $ nf (\l -> foldMap ($ l) $ jobs n formatTest) list
              ]
          ]
  defaultMain [bgroup "Formatting" benches]
