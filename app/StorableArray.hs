{-# LANGUAGE FlexibleContexts #-}

module StorableArray where

import Control.Exception
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter as A
import Data.StorableVector
import Foreign.Storable (Storable)
import System.IO

runExp :: A.Elt e => A.Exp e -> e
runExp e = A.indexArray (A.run (A.unit e)) A.Z

readArrayStorable ::
     (A.Lift A.Exp sh, Storable a, A.Shape sh, A.Shape (A.Plain sh), A.Elt a)
  => sh
  -> FilePath
  -> IO (A.Array sh a)
readArrayStorable sh fp = do
  let n :: Int
      n = runExp . A.shapeSize . A.lift $ sh
  xs <- bracket (openFile fp ReadMode) hClose $ \h -> unpack <$> hGet h n
  return . A.fromList sh $ xs

writeArrayStorable ::
     (A.Shape sh, Storable a, A.Elt a) => A.Array sh a -> FilePath -> IO ()
writeArrayStorable a fp = do
  bracket (openFile fp WriteMode) hClose $ \h -> hPut h . pack $ A.toList a
