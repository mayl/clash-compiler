{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PartialEval.Primitive.Integer
  ( integerPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms
import GHC.Prim
import GHC.Types

import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.Unboxed

integerPrims :: HashMap Text PrimImpl
integerPrims = HashMap.fromList
  [ ("GHC.Integer.Logarithms.integerLogBase#", primIntegerLogBase)
  , ("GHC.Integer.Type.$wsignumInteger", primWSignum)
  , ("GHC.Integer.Type.absInteger", liftUnary absInteger)
  , ("GHC.Integer.Type.andInteger", liftBinary andInteger)
  , ("GHC.Integer.Type.bitInteger", primBitInteger)
  , ("GHC.Integer.Type.compareInteger", liftBinary compareInteger)
  , ("GHC.Integer.Type.complementInteger", liftUnary complementInteger)
  , ("GHC.Integer.Type.czeroBigNat", liftNullary (wordToBigNat (not# 0##)))
  , ("GHC.Integer.Type.decodeDoubleInteger", primDecodeDoubleInteger)
  , ("GHC.Integer.Type.divInteger", liftBinary divInteger)
  , ("GHC.Integer.Type.divModInteger", primDivModInteger)
  , ("GHC.Integer.Type.doubleFromInteger", primDoubleFromInteger)
  , ("GHC.Integer.Type.encodeDoubleInteger", primEncodeDoubleInteger)
  , ("GHC.Integer.Type.encodeFloatInteger", primEncodeFloatInteger)
  , ("GHC.Integer.Type.eqInteger", liftBinary eqInteger)
  , ("GHC.Integer.Type.eqInteger#", integerComparison eqInteger#)
  , ("GHC.Integer.Type.floatFromInteger", primFloatFromInteger)
  , ("GHC.Integer.Type.geInteger", liftBinary geInteger)
  , ("GHC.Integer.Type.geInteger#", integerComparison geInteger#)
  , ("GHC.Integer.Type.gtInteger", liftBinary gtInteger)
  , ("GHC.Integer.Type.gtInteger#", integerComparison gtInteger#)
  , ("GHC.Integer.Type.hashInteger", primHashInteger)
  , ("GHC.Integer.Type.integerToInt", primIntegerToInt)
  , ("GHC.Integer.Type.integerToWord", primIntegerToWord)
  , ("GHC.Integer.Type.leInteger", liftBinary leInteger)
  , ("GHC.Integer.Type.leInteger#", integerComparison leInteger#)
  , ("GHC.Integer.Type.ltInteger", liftBinary ltInteger)
  , ("GHC.Integer.Type.ltInteger#", integerComparison ltInteger#)
  , ("GHC.Integer.Type.minusInteger", liftBinary minusInteger)
  , ("GHC.Integer.Type.modInteger", liftBinary modInteger)
  , ("GHC.Integer.Type.negateInteger", liftUnary negateInteger)
  , ("GHC.Integer.Type.neqInteger", liftBinary neqInteger)
  , ("GHC.Integer.Type.neqInteger#", integerComparison neqInteger#)
  , ("GHC.Integer.Type.nullBigNat", liftUndefined)
  , ("GHC.Integer.Type.oneBigNat", liftNullary oneBigNat)
  , ("GHC.Integer.Type.orInteger", liftBinary orInteger)
  , ("GHC.Integer.Type.plusInteger", liftBinary plusInteger)
  , ("GHC.Integer.Type.quotInteger", liftBinary quotInteger)
  , ("GHC.Integer.Type.quotRemInteger", primQuotRemInteger)
  , ("GHC.Integer.Type.remInteger", liftBinary remInteger)
  , ("GHC.Integer.Type.shiftLInteger", primShiftLInteger)
  , ("GHC.Integer.Type.shiftRInteger", primShiftRInteger)
  , ("GHC.Integer.Type.signumInteger", liftUnary signumInteger)
  , ("GHC.Integer.Type.smallInteger", primSmallInteger)
  , ("GHC.Integer.Type.testBitInteger", primTestBitInteger)
  , ("GHC.Integer.Type.timesInteger", liftBinary timesInteger)
  , ("GHC.Integer.Type.wordToInteger", primWordToInteger)
  , ("GHC.Integer.Type.xorInteger", liftBinary xorInteger)
  , ("GHC.Integer.Type.zeroBigNat", liftNullary zeroBigNat)
  , ("GHC.Integer.bitInteger", primBitInteger)
  ]

primWSignum :: PrimImpl
primWSignum =
  liftUnary $ \x -> UInt (fromInteger (signumInteger x))

primIntegerLogBase :: PrimImpl
primIntegerLogBase =
  liftBinary $ \x y -> UInt (I# (integerLogBase# x y))

primSmallInteger :: PrimImpl
primSmallInteger =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in smallInteger a

primWordToInteger :: PrimImpl
primWordToInteger =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in wordToInteger a

primIntegerToWord :: PrimImpl
primIntegerToWord =
  liftUnary $ \x -> UWord (W# (integerToWord x))

primIntegerToInt :: PrimImpl
primIntegerToInt =
  liftUnary $ \x -> UInt (I# (integerToInt x))

primEncodeFloatInteger :: PrimImpl
primEncodeFloatInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in UFloat (F# (encodeFloatInteger x a))

primFloatFromInteger :: PrimImpl
primFloatFromInteger =
  liftUnary $ \x -> UFloat (F# (floatFromInteger x))

primEncodeDoubleInteger :: PrimImpl
primEncodeDoubleInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in UDouble (D# (encodeDoubleInteger x a))

primDecodeDoubleInteger :: PrimImpl
primDecodeDoubleInteger =
  liftUnary $ \x ->
    let !(UDouble (D# a)) = x
        !(# b, c #) = decodeDoubleInteger a
     in UTuple2 (b, UInt (I# c))

primDoubleFromInteger :: PrimImpl
primDoubleFromInteger =
  liftUnary $ \x -> UDouble (D# (doubleFromInteger x))

primQuotRemInteger :: PrimImpl
primQuotRemInteger =
  liftBinary $ \x y ->
    let !(# a, b #) = quotRemInteger x y
     in UTuple2 (a, b)

primDivModInteger :: PrimImpl
primDivModInteger =
  liftBinary $ \x y ->
    let !(# a, b #) = divModInteger x y
     in UTuple2 (a, b)

primBitInteger :: PrimImpl
primBitInteger =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in bitInteger a

primShiftLInteger :: PrimImpl
primShiftLInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in shiftLInteger x a

primShiftRInteger :: PrimImpl
primShiftRInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in shiftRInteger x a

primTestBitInteger :: PrimImpl
primTestBitInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in testBitInteger x a

primHashInteger :: PrimImpl
primHashInteger =
  liftUnary $ \x -> UInt (I# (hashInteger x))

integerComparison :: (Integer -> Integer -> Int#) -> PrimImpl
integerComparison f =
  liftBinary (\x y -> UInt (I# (f x y)))
