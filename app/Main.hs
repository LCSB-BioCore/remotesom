module Main where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (Z(..), (:.)(..))
import Data.Foldable (foldlM)

import System.Environment

floatArray :: R.Array i sh Float -> R.Array i sh Float
floatArray = id

somIter = undefined
{-
somIter points grid som eps = do
  let diffs = R.traverse2 points som (\(Z :. pts :. dims) (Z :. soms :. _) -> Z :. pts :. soms :. dims)
  pure som
      square x = x * x
  sqdists <- R.sumS $ R.map square diffs
  error "repa can't delay sqdists :("
-}

main :: IO ()
main = do
  (inputFile:restArgs) <- getArgs
  let [dims, cells, somx, somy] = map read (take 4 restArgs) :: [Int]
      iterEps = map read (drop 4 restArgs) :: [Float]
  points <-
    R.transpose . floatArray
      <$> B.readArrayFromStorableFile inputFile (Z :. dims :. cells)
  --print (R.toList $ R.slice points (Z :. (0 :: Int) :. R.All))
  let som =
        floatArray
          $ R.fromFunction
              (Z :. somx * somy :. dims)
              (\(Z :. somn :. dim) -> fromIntegral $ somn - dim)
  finalSom <- foldlM (somIter points undefined) som iterEps
  T.writeMatrixToTextFile "/dev/stdout" finalSom
