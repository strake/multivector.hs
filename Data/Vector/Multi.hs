module Data.Vector.Multi (Vector, MVector) where

import Control.Applicative
import Data.Function (on)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import GHC.Exts (Any)
import Unsafe.Coerce

newtype Vector a = Vector { raw :: V.Vector Any }
newtype MVector s a = MVector { mraw :: V.MVector s Any }

type instance G.Mutable Vector = MVector

instance G.Vector Vector (a, b) where
    basicUnsafeFreeze = fmap Vector . G.basicUnsafeFreeze . mraw
    basicUnsafeThaw = fmap MVector . G.basicUnsafeThaw . raw
    basicLength = (`div` 2) . G.basicLength . raw
    basicUnsafeSlice m n = Vector . G.basicUnsafeSlice (2*m) (2*n) . raw
    basicUnsafeIndexM (Vector xs) k = unsafeCoerce <$> (liftA2 (,) `on` G.basicUnsafeIndexM xs) (2*k) (2*k+1)
    basicUnsafeCopy (MVector xs) (Vector ys) = G.basicUnsafeCopy xs ys

instance MG.MVector MVector (a, b) where
    basicLength = (`div` 2) . MG.basicLength . mraw
    basicUnsafeSlice m n = MVector . MG.basicUnsafeSlice (2*m) (2*n) . mraw
    basicOverlaps = MG.basicOverlaps `on` mraw
    basicUnsafeNew = fmap MVector . MG.basicUnsafeNew
    basicInitialize = MG.basicInitialize . mraw
    basicUnsafeRead xs k = unsafeCoerce <$> (liftA2 (,) `on` MG.basicUnsafeRead xs) (2*k) (2*k+1)
    basicUnsafeWrite (MVector xs) k (a, b) = MG.basicUnsafeWrite xs (2*k)   (unsafeCoerce a) *>
                                             MG.basicUnsafeWrite xs (2*k+1) (unsafeCoerce b)
    basicUnsafeCopy = MG.basicUnsafeCopy `on` mraw
    basicUnsafeMove = MG.basicUnsafeMove `on` mraw
    basicUnsafeGrow (MVector xs) k = MVector <$> MG.basicUnsafeGrow xs (2*k)
