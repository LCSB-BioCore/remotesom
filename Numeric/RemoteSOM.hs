{-
 - Copyright (c) 2025 University of Luxembourg
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Numeric.RemoteSOM where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import Data.Function ((&))

somClosest ::
     A.Acc (A.Matrix Float) -> A.Acc (A.Matrix Float) -> A.Acc (A.Vector Int)
somClosest points som =
  A.zipWith (-) expts exsom
    & A.map (\x -> x * x)
    & A.sum
    & A.imap (\ix v -> A.lift (v, A.indexHead ix))
    & A.minimum
    & A.map A.snd
  where
    (A.I2 pts _) = A.shape points
    (A.I2 somn _) = A.shape som
    expts = A.replicate (A.lift $ Z :. A.All :. somn :. A.All) points
    exsom = A.replicate (A.lift $ Z :. pts :. A.All :. A.All) som

somSumCounts ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Vector Int)
somSumCounts pts som =
  A.lift (s :: A.Acc (A.Matrix Float), c :: A.Acc (A.Vector Int))
  where
    (s, (_ :: A.Acc (A.Matrix Float)), c) = A.unlift $ somSumSqsumCounts pts som

somSumSqsumCounts ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Matrix Float, A.Vector Int)
somSumSqsumCounts points som = A.lift (sums, sqsums, counts)
  where
    (A.I2 pts  _) = A.shape points
    (A.I2 somn dim) = A.shape som
    closest = somClosest points som
    sums =
      A.permute
        (+)
        (A.I2 somn dim `A.fill` A.constant (0 :: Float))
        (\(A.I2 pix dimi) -> A.Just_ $ A.I2 (closest A.! A.I1 pix) dimi)
        points
    sqsums =
      A.permute
        (+)
        (A.I2 somn dim `A.fill` A.constant (0 :: Float))
        (\(A.I2 pix dimi) -> A.Just_ $ A.I2 (closest A.! A.I1 pix) dimi)
        (A.map (\x -> x * x) points)
    counts =
      A.permute
        (+)
        (A.I1 somn `A.fill` A.constant (0 :: Int))
        (\ix -> A.Just_ . A.I1 $ closest A.! ix)
        (A.lift (Z :. pts) `A.fill` A.constant (1 :: Int))

somSmoothWeights ::
     A.Acc (A.Scalar Int)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Scalar Float)
  -> A.Acc (A.Matrix Float)
somSmoothWeights somn gsqdists sigma = weights
  where
    sigma' = A.the $ dodgeZero sigma
    factor = negate $ recip (sigma' * sigma')
    gweights = A.map (\x -> A.exp (x * factor)) gsqdists
    wfactors = A.map A.recip $ dodgeZero $ A.sum gweights
    weights =
      A.zipWith (*) gweights
        $ A.replicate (A.lift $ Z :. A.All :. A.the somn) wfactors

gemm ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
gemm l r =
  let (A.I2 _ oh) = A.shape l
      (A.I2 ow _) = A.shape r
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
    (A.I2 somn dim) = A.shape som
    (sums, counts) = A.unlift $ somSumCounts points som

somLtCounts ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Int)
somLtCounts points som thresholds =
  A.backpermute
    (A.shape points)
    (\(A.I2 pix dimi) -> A.I2 (closest A.! A.I1 pix) dimi)
    thresholds
    & A.zipWith (A.<) points
    & A.map (flip (A.?) (A.constant 1, A.constant 0))
    & A.permute
        (+)
        (A.I2 somn dim `A.fill` A.constant (0 :: Int))
        (\(A.I2 pix dimi) -> A.Just_ $ A.I2 (closest A.! A.I1 pix) dimi)
  where
    somn, dim :: A.Exp Int
    (Z :. somn :. dim) = A.unlift $ A.shape som
    closest = somClosest points som

somMedianInit ::
     A.Acc (A.Scalar Float)
  -> A.Acc (A.Scalar Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Matrix Float)
somMedianInit lb ub som = A.lift (A.fill sh $ A.the lb, A.fill sh $ A.the ub)
  where
    sh = A.shape som

somMedianCountStep ::
     A.Acc (A.Matrix Int)
  -> A.Acc (A.Vector Int)
  -> A.Acc (A.Matrix Float, A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Matrix Float)
somMedianCountStep ltcounts counts bs =
  A.lift
    ( A.zipWith3 (curry . (A.?)) toolow med lbs
    , A.zipWith3 (curry . (A.?)) toolow ubs med)
  where
    (lbs, ubs) = A.unlift bs
    (A.I2 _ dim) = A.shape ltcounts
    med = A.map (/ 2) $ A.zipWith (+) lbs ubs
    toolow =
      A.zipWith
        (A.<)
        (A.map (* 2) ltcounts)
        (A.replicate (A.lift $ Z :. A.All :. dim) counts)
