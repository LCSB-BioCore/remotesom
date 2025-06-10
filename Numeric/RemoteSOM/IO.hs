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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Numeric.RemoteSOM.IO where

import Control.Exception
import qualified Data.Aeson as J
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import qualified Data.Array.Accelerate.Array.Data as A
import Data.Array.Accelerate.IO.Foreign.Ptr
import qualified Data.Array.Accelerate.Representation.Shape as RS
import qualified Data.Array.Accelerate.Sugar.Elt as AE
import qualified Data.Array.Accelerate.Sugar.Shape as AS
import Data.List (intercalate)
import qualified Data.StorableVector as SV
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, sizeOf)
import GHC.Generics
import System.IO
import System.IO.MMap

shapeSize' ::
     forall sh. A.Shape sh
  => sh
  -> Int
shapeSize' = RS.size (AS.shapeR @sh) . AE.fromElt

readArrayStorable ::
     (A.Shape sh, Storable a, A.Elt a) => sh -> FilePath -> IO (A.Array sh a)
readArrayStorable sh fp =
  bracket (openFile fp ReadMode) hClose $ \h ->
    A.fromList sh . SV.unpack <$> SV.hGet h (shapeSize' sh)

writeArrayStorable ::
     (A.Shape sh, Storable a, A.Elt a) => A.Array sh a -> FilePath -> IO ()
writeArrayStorable a fp = do
  bracket (openFile fp WriteMode) hClose $ \h ->
    SV.hPut h . SV.pack $ A.toList a

getFileEntryCount ::
     (A.Shape sh, Storable a) => sh -> a -> FilePath -> IO (Maybe Int)
getFileEntryCount sh a fp = do
  sz <- fromInteger <$> bracket (openFile fp ReadMode) (hClose) hFileSize
  case divMod sz $ shapeSize' sh * sizeOf a of
    (n, 0) -> pure (Just n)
    _ -> pure Nothing

withMmapArray ::
     forall sh a r.
     ( A.Shape sh
     , Storable a
     , A.Elt a
     , A.GArrayDataR Ptr (AE.EltR a) ~ Ptr a
     )
  => sh
  -> FilePath
  -> (A.Array sh a -> IO r)
  -> IO r
withMmapArray sh fp a =
  mmapWithFilePtr fp ReadOnly (Just (0, shapeSize' sh * sizeOf @a undefined))
    $ a . fromPtrs sh . castPtr . fst

readArrayTSV ::
     (A.Shape sh, Read a, A.Elt a) => sh -> FilePath -> IO (A.Array sh a)
readArrayTSV sh fp = A.fromList sh . map read . words <$> readFile fp

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (ys, yss) = splitAt n xs
   in ys : chunks n yss

writeArrayTSV ::
     (A.Shape sh, Show a, A.Elt a) => A.Array (sh :. Int) a -> FilePath -> IO ()
writeArrayTSV a fp =
  let (_ :. rowsize) = A.arrayShape a
   in writeFile fp
        . unlines
        . map (intercalate "\t")
        . chunks rowsize
        . map show
        $ A.toList a

matrixArray :: A.Elt f => A.Matrix f -> [[f]]
matrixArray c = chunks dim $ A.toList c
  where
    (Z :. _ :. dim) = A.arrayShape c

arrayMatrix :: A.Elt f => [[f]] -> A.Matrix f
arrayMatrix c = A.fromList (Z :. nsom :. dim) $ concat c
  where
    nsom = length c
    dim =
      case c of
        (c0:_) -> length c0
        _ -> error "empty matrix"

data DataSummary f = DataSummary
  { dsSums :: [[f]]
  , dsCounts :: [Int]
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)

summaryArray :: A.Elt f => A.Matrix f -> A.Vector Int -> DataSummary f
summaryArray s c =
  DataSummary {dsSums = chunks dim $ A.toList s, dsCounts = A.toList c}
  where
    (Z :. _ :. dim) = A.arrayShape s

arraySummary :: A.Elt f => DataSummary f -> (A.Matrix f, A.Vector Int)
arraySummary DataSummary {dsSums = s, dsCounts = c} =
  (A.fromList (Z :. nsom :. dim) $ concat s, A.fromList (Z :. nsom) c)
  where
    nsom = length s
    dim =
      case s of
        (s0:_) -> length s0
        _ -> error "empty sum"

data DataStatsSummary f = DataStatsSummary
  { dssSums :: [[f]]
  , dssSqsums :: [[f]]
  , dssCounts :: [Int]
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)

statsSummaryArray ::
     A.Elt f => A.Matrix f -> A.Matrix f -> A.Vector Int -> DataStatsSummary f
statsSummaryArray s sq c =
  DataStatsSummary
    { dssSums = chunks dim $ A.toList s
    , dssSqsums = chunks dim $ A.toList sq
    , dssCounts = A.toList c
    }
  where
    (Z :. _ :. dim) = A.arrayShape s

arrayStatsSummary ::
     A.Elt f => DataStatsSummary f -> (A.Matrix f, A.Matrix f, A.Vector Int)
arrayStatsSummary DataStatsSummary {dssSums = s, dssSqsums = sq, dssCounts = c} =
  ( A.fromList (Z :. nsom :. dim) $ concat s
  , A.fromList (Z :. nsom :. dim) $ concat sq
  , A.fromList (Z :. nsom) c)
  where
    nsom = length s
    dim =
      case s of
        (s0:_) -> length s0
        _ -> error "empty sum"

data LtCs = LtCs
  { ltcLess :: [[Int]]
  , ltcCounts :: [Int]
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)

ltCsArray :: A.Matrix Int -> A.Vector Int -> LtCs
ltCsArray l c = LtCs {ltcLess = chunks dim $ A.toList l, ltcCounts = A.toList c}
  where
    (Z :. _ :. dim) = A.arrayShape l

arrayLtCs :: LtCs -> (A.Matrix Int, A.Vector Int)
arrayLtCs LtCs {ltcLess = l, ltcCounts = c} =
  (A.fromList (Z :. nsom :. dim) $ concat l, A.fromList (Z :. nsom) c)
  where
    nsom = length l
    dim =
      case l of
        (l0:_) -> length l0
        _ -> error "empty less"

data Query f
  = QueryDataSummary [[f]]
  | QueryStats [[f]]
  | QueryLessThan [[f]] [[f]]
  deriving (Show, Generic, J.FromJSON, J.ToJSON)

data Shape f = Shape
  { projection :: [[f]]
  , topology :: [[f]]
  } deriving (Show, Generic, J.FromJSON, J.ToJSON)
