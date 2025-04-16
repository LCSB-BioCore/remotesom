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

{-
 - Helper functions
 -}
gemm ::
     A.Acc (A.Matrix Float) -> A.Acc (A.Matrix Float) -> A.Acc (A.Matrix Float)
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

-- well-typed alias
arraySum ::
     (A.Shape sh, A.Elt a, Num (A.Exp a))
  => A.Acc (A.Array sh a)
  -> A.Acc (A.Array sh a)
  -> A.Acc (A.Array sh a)
arraySum = A.zipWith (+)

{-
 - Basic neighborhood-exploring operations
 -}
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
    (A.I2 pts _) = A.shape points
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

{-
 - Neighborhood function application
 -}
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

{-
 - Compound helper that does everything
 -}
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

{-
 - Statistic computation helpers
 -}
somLtCounts ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Vector Int)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Int)
somLtCounts points closest thresholds =
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
    (A.I1 somn) = A.shape closest
    (A.I2 _ dim) = A.shape points

somCounts ::
     A.Acc (A.Scalar Int) -> A.Acc (A.Vector Int) -> A.Acc (A.Vector Int)
somCounts n closest =
  A.permute
    (+)
    (A.I1 (A.the n) `A.fill` A.constant 0)
    (\ix -> A.Just_ $ A.I1 (closest A.! ix))
    (A.shape closest `A.fill` A.constant 1)

somMedianInit ::
     A.Acc (A.Scalar Float)
  -> A.Acc (A.Scalar Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Matrix Float)
somMedianInit lb ub som = A.lift (A.fill sh $ A.the lb, A.fill sh $ A.the ub)
  where
    sh = A.shape som

somMedianMed :: A.Acc (A.Matrix Float, A.Matrix Float) -> A.Acc (A.Matrix Float)
somMedianMed bs = A.map (/ 2) $ A.zipWith (+) lbs ubs
  where
    (lbs, ubs) = A.unlift bs

somMedianCountStep ::
     A.Acc (A.Matrix Int)
  -> A.Acc (A.Vector Int)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Matrix Float)
somMedianCountStep ltcounts counts med bs =
  A.lift
    ( A.zipWith3 (curry . (A.?)) toolow med lbs
    , A.zipWith3 (curry . (A.?)) toolow ubs med)
  where
    (lbs, ubs) = A.unlift bs
    (A.I2 _ dim) = A.shape ltcounts
    toolow =
      A.zipWith
        (A.<)
        (A.map (* 2) ltcounts)
        (A.replicate (A.lift $ Z :. A.All :. dim) counts)

variances ::
     A.Shape sh
  => A.Acc (A.Array sh Float)
  -> A.Acc (A.Array sh Float)
  -> A.Acc (A.Array sh Int)
  -> A.Acc (A.Array sh Float)
variances sums sqsums counts =
  A.zipWith
    (-)
    (means sqsums counts)
    (A.map (\x -> x * x) $ sums `means` counts)

means ::
     A.Shape sh
  => A.Acc (A.Array sh Float)
  -> A.Acc (A.Array sh Int)
  -> A.Acc (A.Array sh Float)
means sums = A.zipWith (/) sums . dodgeZero . A.map A.fromIntegral

somVariances ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Vector Int)
  -> A.Acc (A.Matrix Float)
somVariances sums sqsums counts = variances sums sqsums countss
  where
    (A.I2 _ dim) = A.shape sums
    countss = A.replicate (A.lift $ Z :. A.All :. dim) counts

somMeans ::
     A.Acc (A.Matrix Float) -> A.Acc (A.Vector Int) -> A.Acc (A.Matrix Float)
somMeans sums counts = means sums countss
  where
    (A.I2 _ dim) = A.shape sums
    countss = A.replicate (A.lift $ Z :. A.All :. dim) counts
