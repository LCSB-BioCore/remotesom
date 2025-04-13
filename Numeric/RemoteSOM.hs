{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.RemoteSOM where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import Data.Function ((&))

somSumCounts ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Vector Int)
somSumCounts points som = A.lift (sums, counts)
  where
    pts, somn, dim :: A.Exp Int
    (Z :. pts :. (_ :: A.Exp Int)) = A.unlift $ A.shape points
    (Z :. somn :. dim) = A.unlift $ A.shape som
    expts = A.replicate (A.lift $ Z :. A.All :. somn :. A.All) points
    exsom = A.replicate (A.lift $ Z :. pts :. A.All :. A.All) som
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
        (A.lift (Z :. somn :. dim) `A.fill` A.constant (0 :: Float))
        (\(A.I2 pix dimi) -> A.Just_ $ A.I2 (closest A.! A.I1 pix) dimi)
        points
    counts =
      A.permute
        (+)
        (A.lift (Z :. somn) `A.fill` A.constant (0 :: Int))
        (\ix -> A.Just_ . A.I1 $ closest A.! ix)
        (A.lift (Z :. pts) `A.fill` A.constant (1 :: Int))

somSmoothWeights ::
     A.Acc (A.Scalar Int)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Scalar Float)
  -> A.Acc (A.Matrix Float)
somSmoothWeights somn gsqdists sigma = weights
  where
    sigma' = dodgeZero sigma
    factor = negate . recip $ A.the sigma' * A.the sigma'
    gweights = A.map (\x -> A.exp (x * factor)) gsqdists
    wfactors = A.map A.recip $ dodgeZero $ A.sum gweights
    weights =
      A.zipWith (*) gweights
        $ A.replicate (A.lift $ Z :. A.All :. A.the somn) wfactors

gemm ::
     A.Acc (A.Array (Z :. Int :. Int) Float)
  -> A.Acc (A.Array (Z :. Int :. Int) Float)
  -> A.Acc (A.Array (Z :. Int :. Int) Float)
gemm l r =
  let ow, oh :: A.Exp Int
      (Z :. (_ :: A.Exp Int) :. oh) = A.unlift (A.shape l)
      (Z :. ow :. (_ :: A.Exp Int)) = A.unlift (A.shape r)
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
     A.Acc (A.Scalar Int)
  -> A.Acc (A.Scalar Int)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Vector Int)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Scalar Float)
  -> A.Acc (A.Matrix Float)
somAggregate somn dim sums counts gsqdists sigma =
  A.zipWith (/) sumsS (dodgeZero countsS)
  where
    smoothWeights = somSmoothWeights somn gsqdists sigma
    sumsS = gemm sums smoothWeights
    countsS =
      gemm
        (A.replicate (A.lift $ Z :. A.All :. A.the dim)
           $ A.map A.fromIntegral counts)
        smoothWeights

arraySum ::
     (A.Shape sh, A.Elt a, Num (A.Exp a))
  => A.Acc (A.Array sh a)
  -> A.Acc (A.Array sh a)
  -> A.Acc (A.Array sh a)
arraySum = A.zipWith (+)

somIter ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Scalar Float)
  -> A.Acc (A.Matrix Float)
somIter points gsqdists som sigma =
  somAggregate (A.unit somn) (A.unit dim) sums counts gsqdists sigma
  where
    somn, dim :: A.Exp Int
    (Z :. somn :. dim) = A.unlift $ A.shape som
    (sums, counts) = A.unlift $ somSumCounts points som
