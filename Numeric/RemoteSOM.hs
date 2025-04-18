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
 -
 - somClosest, seqArgMin and seqSum was kindly provided by Tom Smeding (thanks!)
 -}
somClosest ::
     A.Acc (A.Matrix Float) -> A.Acc (A.Matrix Float) -> A.Acc (A.Vector Int)
somClosest points som =
  let A.I2 pts dim = A.shape points
      A.I2 somn _ = A.shape som
   in A.generate (A.I1 pts) $ \(A.I1 pix) ->
        seqArgMin1 somn $ \somi ->
          seqSum dim $ \dimi ->
            (points A.! A.I2 pix dimi - som A.! A.I2 somi dimi) ^ (2 :: Int)

-- assumes n >= 1
seqArgMin1 :: A.Ord a => A.Exp Int -> (A.Exp Int -> A.Exp a) -> A.Exp Int
seqArgMin1 n f =
  let A.T3 _ res _ =
        A.while
          (\(A.T3 i _ _) -> i A.< n)
          (\(A.T3 i mini minv) ->
             let v' = f i
              in A.cond
                   (v' A.< minv)
                   (A.T3 (i + 1) i v')
                   (A.T3 (i + 1) mini minv))
          (A.T3 1 0 (f 0))
   in res

seqSum :: A.Num a => A.Exp Int -> (A.Exp Int -> A.Exp a) -> A.Exp a
seqSum n f =
  A.snd
    $ A.while
        ((A.< n) . A.fst)
        (\(A.T2 i s) -> A.T2 (i + 1) (s + f i))
        (A.T2 0 0)

somSums ::
     A.Acc (A.Scalar Int)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Vector Int)
  -> A.Acc (A.Matrix Float)
somSums somn points closest =
  A.permute
    (+)
    (A.I2 (A.the somn) dim `A.fill` A.constant 0)
    (\(A.I2 pix dimi) -> A.Just_ $ A.I2 (closest A.! A.I1 pix) dimi)
    points
  where
    (A.I2 _ dim) = A.shape points

somSqsums ::
     A.Acc (A.Scalar Int)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Vector Int)
  -> A.Acc (A.Matrix Float)
somSqsums somn points closest =
  A.permute
    (+)
    (A.I2 (A.the somn) dim `A.fill` A.constant 0)
    (\(A.I2 pix dimi) -> A.Just_ $ A.I2 (closest A.! A.I1 pix) dimi)
    (A.map (\x -> x * x) points)
  where
    (A.I2 _ dim) = A.shape points

somCounts ::
     A.Acc (A.Scalar Int) -> A.Acc (A.Vector Int) -> A.Acc (A.Vector Int)
somCounts somn closest =
  A.permute
    (+)
    (A.I1 (A.the somn) `A.fill` A.constant 0)
    (\ix -> A.Just_ $ A.I1 (closest A.! ix))
    (A.shape closest `A.fill` A.constant 1)

somSumCounts ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Vector Int)
somSumCounts points som =
  A.lift (somSums somn points closest, somCounts somn closest)
  where
    (A.I2 somn' _) = A.shape som
    somn = A.unit somn'
    closest = somClosest points som

somSumSqsumCounts ::
     A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float)
  -> A.Acc (A.Matrix Float, A.Matrix Float, A.Vector Int)
somSumSqsumCounts points som =
  A.lift
    ( somSums somn points closest
    , somSqsums somn points closest
    , somCounts somn closest)
  where
    (A.I2 somn' _) = A.shape som
    somn = A.unit somn'
    closest = somClosest points som

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
    (A.I2 somn dim) = A.shape thresholds

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
