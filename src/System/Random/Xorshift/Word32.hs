{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module System.Random.Xorshift.Word32 where

import Data.Bits
import Data.STRef
import Control.Monad.ST
import Data.Word
import Data.Coerce

newtype XorShiftState = XorShiftState Word32
  deriving stock (Eq)
  deriving newtype Bits

data Triple = Triple {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int


xorshift :: Triple -> STRef s XorShiftState -> ST s Word32
xorshift (Triple a b c) stateRef = do
  modifySTRef' stateRef (\x -> x `xor` (x `shiftL` a))
  modifySTRef' stateRef (\x -> x `xor` (x `shiftR` b))
  modifySTRef' stateRef (\x -> x `xor` (x `shiftL` c))
  coerce <$> readSTRef stateRef


data XorWowState = XorWowState
  { a       :: {-# UNPACK #-} !Word32
  , b       :: {-# UNPACK #-} !Word32
  , c       :: {-# UNPACK #-} !Word32
  , d       :: {-# UNPACK #-} !Word32
  , counter :: {-# UNPACK #-} !Word32
  }

xorwowShift :: STRef s XorWowState -> ST s Word32
xorwowShift stateRef = do
  XorWowState {..} <- readSTRef stateRef
  tRef <- newSTRef d
  modifySTRef' tRef (\x -> x `xor` (x `shiftR` 2))
  modifySTRef' tRef (\x -> x `xor` (x `shiftL` 1))
  modifySTRef' tRef (\x -> x `xor` (a ^ (a `shiftL` 4)))
  t <- readSTRef tRef
  let newCounter = counter + 362437
  writeSTRef stateRef (XorWowState t a b c newCounter)
  pure (t + newCounter)
  

  
