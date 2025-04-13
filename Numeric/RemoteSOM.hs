{-# LANGUAGE TypeOperators #-}

module Numeric.RemoteSOM where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import Data.Function ((&))

somSumCounts ::
     A.Matrix Float
  -> A.Matrix Float
  -> (A.Acc (A.Matrix Float), A.Acc (A.Vector Int))
somSumCounts points som = (sums, counts)
  where
    pts, somn :: Int
    (Z :. pts :. _) = A.arrayShape points
    (Z :. somn :. dim) = A.arrayShape som
    expts =
      A.replicate (A.constant $ Z :. A.All :. somn :. A.All) $ A.use points
    exsom = A.replicate (A.constant $ Z :. pts :. A.All :. A.All) $ A.use som
    closest =
      A.zipWith (-) expts exsom
        & A.map (\x -> x * x)
        & A.sum
        & A.imap (\ix v -> A.lift (v, A.indexHead ix))
        & A.minimum
        & A.map A.snd
    sums =
      A.permute
        (+)
        (A.constant (Z :. somn :. dim) `A.fill` A.constant (0 :: Float))
        (\(A.I2 pix dimi) -> A.Just_ $ A.I2 (closest A.! A.I1 pix) dimi)
        (A.use points)
    counts =
      A.permute
        (+)
        (A.constant (Z :. somn) `A.fill` A.constant (0 :: Int))
        (\ix -> A.Just_ . A.I1 $ closest A.! ix)
        (A.constant (Z :. pts) `A.fill` A.constant (1 :: Int))

somSmoothWeights ::
     Int -> A.Acc (A.Matrix Float) -> Float -> A.Acc (A.Matrix Float)
somSmoothWeights somn gsqdists sigma = weights
  where
    factor = (-1) / (sigma * sigma)
    gweights = A.map (\x -> A.exp (x * A.constant factor)) gsqdists
    wfactors = A.map A.recip $ dodgeZero $ A.sum gweights
    weights =
      A.zipWith (*) gweights
        $ A.replicate (A.constant (Z :. A.All :. (somn :: Int))) wfactors

gemm ::
     A.Acc (A.Array (Z :. Int :. Int) Float)
  -> A.Acc (A.Array (Z :. Int :. Int) Float)
  -> A.Acc (A.Array (Z :. Int :. Int) Float)
gemm l r =
  let lw, lh, rw, rh :: A.Exp Int
      (Z :. lw :. lh) = A.unlift (A.shape l)
      (Z :. rw :. rh) = A.unlift (A.shape r)
      (ow, oh, dim) = (rw, lh, rh)
   in A.sum
        $ A.zipWith
            (*)
            (A.replicate (A.lift $ Z :. ow :. A.All :. A.All) $ A.transpose l)
            (A.replicate (A.lift $ Z :. A.All :. oh :. A.All) r)

epsilon :: Float
epsilon = 1e-7

dodgeZero ::
     (A.Shape sh) => A.Acc (A.Array sh Float) -> A.Acc (A.Array sh Float)
dodgeZero x = A.zipWith A.max (A.shape x `A.fill` A.constant epsilon) x

somAggregate ::
     Int
  -> Int
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Vector Int)
  -> A.Acc (A.Matrix Float)
  -> Float
  -> A.Acc (A.Matrix Float)
somAggregate somn dim sums counts gsqdists sigma =
  A.zipWith (/) sumsS (dodgeZero countsS)
  where
    smoothWeights = somSmoothWeights somn gsqdists sigma
    sumsS = gemm sums smoothWeights
    countsS :: A.Acc (A.Array (Z :. Int :. Int) Float)
    countsS =
      gemm
        (A.replicate (A.constant $ Z :. A.All :. dim)
           $ A.map A.fromIntegral counts)
        smoothWeights

somIter ::
     A.Matrix Float
  -> A.Acc (A.Matrix Float)
  -> A.Matrix Float
  -> Float
  -> A.Acc (A.Matrix Float)
somIter points gsqdists som sigma =
  somAggregate somn dim sums counts gsqdists sigma
  where
    (Z :. somn :. dim) = A.arrayShape som
    (sums, counts) = somSumCounts points som
