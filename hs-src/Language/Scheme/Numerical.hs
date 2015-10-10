{-# LANGUAGE FlexibleContexts #-}
{- |
Module      : Language.Scheme.Numerical
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module implements the numerical tower.
-}

module Language.Scheme.Numerical (
 -- * Generic functions
   numSub
 , numMul
 , numDiv
 , numAdd
 , numMod
 , numRationalize
 , numBoolBinopEq
 , numBoolBinopGt
 , numBoolBinopGte
 , numBoolBinopLt
 , numBoolBinopLte
 , numCast
 , numDenominator
 , numNumerator
 , numInexact2Exact
 , numExact2Inexact 
 , num2String
 , unpackNum
 , numericBinop
 -- * Floating point functions
 , numFloor
 , numCeiling
 , numTruncate
 , numRound
 , numExpt
 , numSqrt
 , numExp
 , numLog
 -- * Trigonometric functions
 , numSin
 , numCos
 , numTan
 , numAsin 
 , numAcos
 , numAtan
 -- * Complex functions
 , buildComplex
 , numMakePolar
 , numRealPart
 , numImagPart
 , numMagnitude
 , numAngle
 , numMakeRectangular
 -- * Predicates
 , isComplex
 , isReal 
 , isRational
 , isInteger
 , isNumber
 , isFloatAnInteger
 , isNumNaN
 , isNumInfinite
 , isNumFinite
 , isNumExact
 , isNumInexact
) where
import Language.Scheme.Types

import Control.Monad.Except
import Data.Char hiding (isNumber)
import Data.Complex
import Data.Fixed
import Data.Ratio
import Numeric
import Text.Printf

-- |A helper function to perform a numeric operation on two values
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal r] -> ThrowsError r (LispVal r)
numericBinop _ singleVal@[_] = throwError $ NumArgs (Just 2) singleVal
numericBinop op aparams = mapM unpackNum aparams >>= return . Number . foldl1 op

-- - Begin GenUtil - http://repetae.net/computer/haskell/GenUtil.hs
foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM f v (x : xs) = (f v x) >>= \ a -> foldlM f a xs
foldlM _ v [] = return v

foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldl1M f (x : xs) = foldlM f x xs
foldl1M _ _ = error "Unexpected error in foldl1M"
-- end GenUtil


{- FUTURE: as a general comment here, operations need to be more permissive of the
numerical types they accept. Within reason, a user should not have to know
what numerical type they are passing when using these functions -}

-- |Add the given numbers
numAdd :: [LispVal r] -> ThrowsError r (LispVal r)
numAdd [] = return $ Number 0
numAdd aparams = do
  foldl1M (\ a b -> doAdd =<< (numCast [a, b])) aparams
  where doAdd (List [(Number a), (Number b)]) = return $ Number $ a + b
        doAdd (List [(Float a), (Float b)]) = return $ Float $ a + b
        doAdd (List [(Rational a), (Rational b)]) = return $ Rational $ a + b
        doAdd (List [(Complex a), (Complex b)]) = return $ Complex $ a + b
        doAdd _ = throwError $ Default "Unexpected error in +"

-- |Subtract the given numbers
numSub :: [LispVal r] -> ThrowsError r (LispVal r)
numSub [] = throwError $ NumArgs (Just 1) []
numSub [Number n] = return $ Number $ -1 * n
numSub [Float n] = return $ Float $ -1 * n
numSub [Rational n] = return $ Rational $ -1 * n
numSub [Complex n] = return $ Complex $ -1 * n
numSub aparams = do
  foldl1M (\ a b -> doSub =<< (numCast [a, b])) aparams
  where doSub (List [(Number a), (Number b)]) = return $ Number $ a - b
        doSub (List [(Float a), (Float b)]) = return $ Float $ a - b
        doSub (List [(Rational a), (Rational b)]) = return $ Rational $ a - b
        doSub (List [(Complex a), (Complex b)]) = return $ Complex $ a - b
        doSub _ = throwError $ Default "Unexpected error in -"

-- |Multiply the given numbers
numMul :: [LispVal r] -> ThrowsError r (LispVal r)
numMul [] = return $ Number 1
numMul aparams = do
  foldl1M (\ a b -> doMul =<< (numCast [a, b])) aparams
  where doMul (List [(Number a), (Number b)]) = return $ Number $ a * b
        doMul (List [(Float a), (Float b)]) = return $ Float $ a * b
        doMul (List [(Rational a), (Rational b)]) = return $ Rational $ a * b
        doMul (List [(Complex a), (Complex b)]) = return $ Complex $ a * b
        doMul _ = throwError $ Default "Unexpected error in *"

-- |Divide the given numbers
numDiv :: [LispVal r] -> ThrowsError r (LispVal r)
numDiv [] = throwError $ NumArgs (Just 1) []
numDiv [Number 0] = throwError $ DivideByZero 
numDiv [Rational 0] = throwError $ DivideByZero  
numDiv [Number n] = return $ Rational $ 1 / (fromInteger n)
numDiv [Float n] = return $ Float $ 1.0 / n
numDiv [Rational n] = return $ Rational $ 1 / n
numDiv [Complex n] = return $ Complex $ 1 / n
numDiv aparams = do
  foldl1M (\ a b -> doDiv =<< (numCast [a, b])) aparams
  where doDiv (List [(Number a), (Number b)])
            | b == 0 = throwError $ DivideByZero
            | (mod a b) == 0 = return $ Number $ div a b
            | otherwise = -- Not an integer
                return $ Rational $ (fromInteger a) / (fromInteger b)
        doDiv (List [(Float a), (Float b)]) 
            | b == 0.0 = throwError $ DivideByZero
            | otherwise = return $ Float $ a / b
        doDiv (List [(Rational a), (Rational b)])
            | b == 0 = throwError $ DivideByZero
            | otherwise = return $ Rational $ a / b
        doDiv (List [(Complex a), (Complex b)])
            | b == 0 = throwError $ DivideByZero
            | otherwise = return $ Complex $ a / b
        doDiv _ = throwError $ Default "Unexpected error in /"

-- |Take the modulus of the given numbers
numMod :: [LispVal r] -> ThrowsError r (LispVal r)
numMod [] = return $ Number 1
numMod aparams = do
  foldl1M (\ a b -> doMod =<< (numCast [a, b])) aparams
  where doMod (List [(Number a), (Number b)]) = return $ Number $ mod' a b
        doMod (List [(Float a), (Float b)]) = return $ Float $ mod' a b
        doMod (List [(Rational a), (Rational b)]) = return $ Rational $ mod' a b
        doMod (List [(Complex _), (Complex _)]) = throwError $ Default "modulo not implemented for complex numbers" 
        doMod _ = throwError $ Default "Unexpected error in modulo"

-- |Compare a series of numbers using a given numeric comparison
--  function and an array of lisp values
numBoolBinopCompare :: (LispVal r
                    -> LispVal r -> ThrowsError r (LispVal r))
                    -> LispVal r -> [LispVal r] -> ThrowsError r (LispVal r)
numBoolBinopCompare cmp n1 (n2 : ns) = do
  List [n1', n2'] <- numCast [n1, n2]
  result <- cmp n1' n2'
  case result of
    Bool True -> numBoolBinopCompare cmp n2' ns
    _ -> return $ Bool False
numBoolBinopCompare _ _ _ = return $ Bool True

-- |Numeric equals
numBoolBinopEq :: [LispVal r] -> ThrowsError r (LispVal r)
numBoolBinopEq [] = throwError $ NumArgs (Just 0) []
numBoolBinopEq (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a == b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Rational a) (Rational b) = return $ Bool $ f a b
    cmp (Complex a) (Complex b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in ="

-- |Numeric greater than
numBoolBinopGt :: [LispVal r] -> ThrowsError r (LispVal r)
numBoolBinopGt [] = throwError $ NumArgs (Just 0) []
numBoolBinopGt (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a > b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Rational a) (Rational b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in >"

-- |Numeric greater than equal
numBoolBinopGte :: [LispVal r] -> ThrowsError r (LispVal r)
numBoolBinopGte [] = throwError $ NumArgs (Just 0) []
numBoolBinopGte (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a >= b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Rational a) (Rational b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in >="

-- |Numeric less than 
numBoolBinopLt :: [LispVal r] -> ThrowsError r (LispVal r)
numBoolBinopLt [] = throwError $ NumArgs (Just 0) []
numBoolBinopLt (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a < b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Rational a) (Rational b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in <"

-- |Numeric less than equal
numBoolBinopLte :: [LispVal r] -> ThrowsError r (LispVal r)
numBoolBinopLte [] = throwError $ NumArgs (Just 0) []
numBoolBinopLte (n : ns) = numBoolBinopCompare cmp n ns
  where
    f a b = a <= b
    cmp (Number a) (Number b) = return $ Bool $ f a b
    cmp (Float a) (Float b) = return $ Bool $ f a b
    cmp (Rational a) (Rational b) = return $ Bool $ f a b
    cmp _ _ = throwError $ Default "Unexpected error in <="

-- |Accept two numbers and cast one of them to the appropriate type, if necessary
numCast :: [LispVal r] -> ThrowsError r (LispVal r)
numCast [a@(Number _), b@(Number _)] = return $ List [a, b]
numCast [a@(Float _), b@(Float _)] = return $ List [a, b]
numCast [a@(Rational _), b@(Rational _)] = return $ List [a, b]
numCast [a@(Complex _), b@(Complex _)] = return $ List [a, b]
numCast [(Number a), b@(Float _)] = return $ List [Float $ fromInteger a, b]
numCast [(Number a), b@(Rational _)] = return $ List [Rational $ fromInteger a, b]
numCast [(Number a), b@(Complex _)] = return $ List [Complex $ fromInteger a, b]
numCast [a@(Float _), (Number b)] = return $ List [a, Float $ fromInteger b]
numCast [a@(Float _), (Rational b)] = return $ List [a, Float $ fromRational b]
numCast [(Float a), b@(Complex _)] = return $ List [Complex $ a :+ 0, b]
numCast [a@(Rational _), (Number b)] = return $ List [a, Rational $ fromInteger b]
numCast [(Rational a), b@(Float _)] = return $ List [Float $ fromRational a, b]
numCast [(Rational a), b@(Complex _)] = return $ List [Complex $ (fromInteger $ numerator a) / (fromInteger $ denominator a), b]
numCast [a@(Complex _), (Number b)] = return $ List [a, Complex $ fromInteger b]
numCast [a@(Complex _), (Float b)] = return $ List [a, Complex $ b :+ 0]
numCast [a@(Complex _), (Rational b)] = return $ List [a, Complex $ (fromInteger $ numerator b) / (fromInteger $ denominator b)]
numCast [a, b] = case a of
               Number _ -> doThrowError b
               Float _ -> doThrowError b
               Rational _ -> doThrowError b
               Complex _ -> doThrowError b
               _ -> doThrowError a
  where doThrowError num = throwError $ TypeMismatch "number" num
numCast _ = throwError $ Default "Unexpected error in numCast"

-- |Convert the given number to a rational
numRationalize :: [LispVal r] -> ThrowsError r (LispVal r)
numRationalize [(Number n)] = return $ Rational $ toRational n
numRationalize [(Float n)] = return $ Rational $ toRational n
numRationalize [n@(Rational _)] = return n
numRationalize [x] = throwError $ TypeMismatch "number" x
numRationalize badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Round the given number
numRound :: [LispVal r] -> ThrowsError r (LispVal r)
numRound [n@(Number _)] = return n
numRound [(Rational n)] = return $ Number $ round n
numRound [(Float n)] = return $ Float $ fromInteger $ round n
numRound [(Complex n)] = do
  return $ Complex $ (fromInteger $ round $ realPart n) :+ (fromInteger $ round $ imagPart n)
numRound [x] = throwError $ TypeMismatch "number" x
numRound badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Floor the given number
numFloor :: [LispVal r] -> ThrowsError r (LispVal r)
numFloor [n@(Number _)] = return n
numFloor [(Rational n)] = return $ Number $ floor n
numFloor [(Float n)] = return $ Float $ fromInteger $ floor n
numFloor [(Complex n)] = do
  return $ Complex $ (fromInteger $ floor $ realPart n) :+ (fromInteger $ floor $ imagPart n)
numFloor [x] = throwError $ TypeMismatch "number" x
numFloor badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Take the ceiling of the given number
numCeiling :: [LispVal r] -> ThrowsError r (LispVal r)
numCeiling [n@(Number _)] = return n
numCeiling [(Rational n)] = return $ Number $ ceiling n
numCeiling [(Float n)] = return $ Float $ fromInteger $ ceiling n
numCeiling [(Complex n)] = do
  return $ Complex $ (fromInteger $ ceiling $ realPart n) :+ (fromInteger $ ceiling $ imagPart n)
numCeiling [x] = throwError $ TypeMismatch "number" x
numCeiling badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Truncate the given number
numTruncate :: [LispVal r] -> ThrowsError r (LispVal r)
numTruncate [n@(Number _)] = return n
numTruncate [(Rational n)] = return $ Number $ truncate n
numTruncate [(Float n)] = return $ Float $ fromInteger $ truncate n
numTruncate [(Complex n)] = do
  return $ Complex $ (fromInteger $ truncate $ realPart n) :+ (fromInteger $ truncate $ imagPart n)
numTruncate [x] = throwError $ TypeMismatch "number" x
numTruncate badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Sine
numSin :: [LispVal r] -> ThrowsError r (LispVal r)
numSin [(Number n)] = return $ Float $ sin $ fromInteger n
numSin [(Float n)] = return $ Float $ sin n
numSin [(Rational n)] = return $ Float $ sin $ fromRational n
numSin [(Complex n)] = return $ Complex $ sin n
numSin [x] = throwError $ TypeMismatch "number" x
numSin badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Cosine
numCos :: [LispVal r] -> ThrowsError r (LispVal r)
numCos [(Number n)] = return $ Float $ cos $ fromInteger n
numCos [(Float n)] = return $ Float $ cos n
numCos [(Rational n)] = return $ Float $ cos $ fromRational n
numCos [(Complex n)] = return $ Complex $ cos n
numCos [x] = throwError $ TypeMismatch "number" x
numCos badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Tangent
numTan :: [LispVal r] -> ThrowsError r (LispVal r)
numTan [(Number n)] = return $ Float $ tan $ fromInteger n
numTan [(Float n)] = return $ Float $ tan n
numTan [(Rational n)] = return $ Float $ tan $ fromRational n
numTan [(Complex n)] = return $ Complex $ tan n
numTan [x] = throwError $ TypeMismatch "number" x
numTan badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Arcsine
numAsin :: [LispVal r] -> ThrowsError r (LispVal r)
numAsin [(Number n)] = return $ Float $ asin $ fromInteger n
numAsin [(Float n)] = return $ Float $ asin n
numAsin [(Rational n)] = return $ Float $ asin $ fromRational n
numAsin [(Complex n)] = return $ Complex $ asin n
numAsin [x] = throwError $ TypeMismatch "number" x
numAsin badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Arccosine
numAcos :: [LispVal r] -> ThrowsError r (LispVal r)
numAcos [(Number n)] = return $ Float $ acos $ fromInteger n
numAcos [(Float n)] = return $ Float $ acos n
numAcos [(Rational n)] = return $ Float $ acos $ fromRational n
numAcos [(Complex n)] = return $ Complex $ acos n
numAcos [x] = throwError $ TypeMismatch "number" x
numAcos badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Arctangent
numAtan :: [LispVal r] -> ThrowsError r (LispVal r)
numAtan [(Number n)] = return $ Float $ atan $ fromInteger n
numAtan [Number y, Number x] = return $ Float $ phase $ (fromInteger x) :+ (fromInteger y)
numAtan [(Float n)] = return $ Float $ atan n
numAtan [Float y, Float x] = return $ Float $ phase $ x :+ y
numAtan [(Rational n)] = return $ Float $ atan $ fromRational n
numAtan [Rational y, Rational x] = return $ Float $ phase $ (fromRational x) :+ (fromRational y)
numAtan [(Complex n)] = return $ Complex $ atan n
numAtan [x] = throwError $ TypeMismatch "number" x
numAtan badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Take the square root of the given number
numSqrt :: [LispVal r] -> ThrowsError r (LispVal r)
numSqrt [(Number n)] = if n >= 0 then return $ Float $ sqrt $ fromInteger n
                                 else return $ Complex $ sqrt ((fromInteger n) :+ 0)
numSqrt [(Float n)] = if n >= 0 then return $ Float $ sqrt n
                                else return $ Complex $ sqrt (n :+ 0)
numSqrt [(Rational n)] = numSqrt [Float $ fromRational n]
numSqrt [(Complex n)] = return $ Complex $ sqrt n
numSqrt [x] = throwError $ TypeMismatch "number" x
numSqrt badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Raise the first number to the power of the second
numExpt :: [LispVal r] -> ThrowsError r (LispVal r)
numExpt [(Number n), (Number p)] = return $ Float $ (fromInteger n) ^ p
numExpt [(Rational n), (Number p)] = return $ Float $ (fromRational n) ^ p
numExpt [(Float n), (Number p)] = return $ Float $ n ^ p
numExpt [(Complex n), (Number p)] = return $ Complex $ n ^ p
numExpt [_, y] = throwError $ TypeMismatch "integer" y
numExpt badArgList = throwError $ NumArgs (Just 2) badArgList

{- numExpt params = do
  foldl1M (\a b -> doExpt =<< (numCast [a, b])) params
  where doExpt (List [(Number a), (Number b)]) = return $ Float $ (fromInteger a) ^ (fromInteger b)
--        doExpt (List [(Rational a), (Rational b)]) = return $ Float $ fromRational $ a ^ b
        doExpt (List [(Float a), (Float b)]) = return $ Float $ a ^ b
--        doExpt (List [(Complex a), (Complex b)]) = return $ Complex $ a ^ b -}

-- |Take the exponent of the given number
numExp :: [LispVal r] -> ThrowsError r (LispVal r)
numExp [(Number n)] = return $ Float $ exp $ fromInteger n
numExp [(Float n)] = return $ Float $ exp n
numExp [(Rational n)] = return $ Float $ exp $ fromRational n
numExp [(Complex n)] = return $ Complex $ exp n
numExp [x] = throwError $ TypeMismatch "number" x
numExp badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Compute the log of a given number
numLog :: [LispVal r] -> ThrowsError r (LispVal r)
numLog [(Number n)] = return $ Float $ log $ fromInteger n
numLog [Number n, Number base] = return $ Float $ logBase (fromInteger base) (fromInteger n)
numLog [(Float n)] = return $ Float $ log n
numLog [Float n, Number base] = return $ Float $ logBase (fromInteger base) n
numLog [(Rational n)] = return $ Float $ log $ fromRational n
numLog [Rational n, Number base] = return $ Float $ logBase (fromInteger base) (fromRational n)
numLog [(Complex n)] = return $ Complex $ log n
numLog [Complex n, Number base] = return $ Complex $ logBase (fromInteger base) n
numLog [x] = throwError $ TypeMismatch "number" x
numLog badArgList = throwError $ NumArgs (Just 1) badArgList

-- Complex number functions

-- |Create a complex number
buildComplex :: LispVal r 
             -- ^ Real part
             -> LispVal r 
             -- ^ Imaginary part
             -> ThrowsError r (LispVal r)
             -- ^ Complex number
buildComplex (Number x) (Number y) = return $ Complex $ (fromInteger x) :+ (fromInteger y)
buildComplex (Number x) (Rational y) = return $ Complex $ (fromInteger x) :+ (fromRational y)
buildComplex (Number x) (Float y) = return $ Complex $ (fromInteger x) :+ y
buildComplex (Rational x) (Number y) = return $ Complex $ (fromRational x) :+ (fromInteger y)
buildComplex (Rational x) (Rational y) = return $ Complex $ (fromRational x) :+ (fromRational y)
buildComplex (Rational x) (Float y) = return $ Complex $ (fromRational x) :+ y
buildComplex (Float x) (Number y) = return $ Complex $ x :+ (fromInteger y)
buildComplex (Float x) (Rational y) = return $ Complex $ x :+ (fromRational y)
buildComplex (Float x) (Float y) = return $ Complex $ x :+ y
buildComplex x y = throwError $ TypeMismatch "number" $ List [x, y]

-- |Create a complex number given its real and imaginary parts
numMakeRectangular :: [LispVal r] -> ThrowsError r (LispVal r)
numMakeRectangular [x, y] = buildComplex x y
numMakeRectangular badArgList = throwError $ NumArgs (Just 2) badArgList

-- |Create a complex number from its magnitude and phase (angle)
numMakePolar :: [LispVal r] -> ThrowsError r (LispVal r)
numMakePolar [(Float x), (Float y)] = return $ Complex $ mkPolar x y
numMakePolar [(Float _), y] = throwError $ TypeMismatch "real" y
numMakePolar [x, (Float _)] = throwError $ TypeMismatch "real real" $ x
numMakePolar badArgList = throwError $ NumArgs (Just 2) badArgList

-- |The phase of a complex number
numAngle :: [LispVal r] -> ThrowsError r (LispVal r)
numAngle [(Complex c)] = return $ Float $ phase c
numAngle [x] = throwError $ TypeMismatch "complex number" x
numAngle badArgList = throwError $ NumArgs (Just 1) badArgList

-- |The nonnegative magnitude of a complex number
numMagnitude :: [LispVal r] -> ThrowsError r (LispVal r)
numMagnitude [(Complex c)] = return $ Float $ magnitude c
numMagnitude [x] = throwError $ TypeMismatch "complex number" x
numMagnitude badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Retrieve real part of a complex number
numRealPart :: [LispVal r] -> ThrowsError r (LispVal r)
numRealPart [(Complex c)] = return $ Float $ realPart c
numRealPart [n@(Float _)] = return n
numRealPart [n@(Rational _)] = return n
numRealPart [n@(Number _)] = return n
numRealPart [x] = throwError $ TypeMismatch "complex number" x
numRealPart badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Retrieve imaginary part of a complex number
numImagPart :: [LispVal r] -> ThrowsError r (LispVal r)
numImagPart [(Complex c)] = do
  let n = imagPart c
      f = Float n
  if isFloatAnInteger f
     then return $ Number $ floor n
     else return f
numImagPart [(Float _)] = return $ Number 0
numImagPart [(Rational _)] = return $ Number 0
numImagPart [(Number _)] = return $ Number 0
numImagPart [x] = throwError $ TypeMismatch "complex number" x
numImagPart badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Take the numerator of the given number
numNumerator :: [LispVal r] -> ThrowsError r (LispVal r)
numNumerator [n@(Number _)] = return n
numNumerator [(Rational r)] = return $ Number $ numerator r
numNumerator [(Float f)] = return $ Float $ fromInteger . numerator . toRational $ f
numNumerator [x] = throwError $ TypeMismatch "rational number" x
numNumerator badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Take the denominator of the given number
numDenominator :: [LispVal r] -> ThrowsError r (LispVal r)
numDenominator [Number _] = return $ Number 1
numDenominator [(Rational r)] = return $ Number $ denominator r
numDenominator [(Float f)] = return $ Float $ fromInteger $ denominator $ toRational f
numDenominator [x] = throwError $ TypeMismatch "rational number" x
numDenominator badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Convert an exact number to inexact
numExact2Inexact :: [LispVal r] -> ThrowsError r (LispVal r)
numExact2Inexact [(Number n)] = return $ Float $ fromInteger n
numExact2Inexact [(Rational n)] = return $ Float $ fromRational n
numExact2Inexact [n@(Float _)] = return n
numExact2Inexact [n@(Complex _)] = return n
numExact2Inexact [badType] = throwError $ TypeMismatch "number" badType
numExact2Inexact badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Convert an inexact number to exact
numInexact2Exact :: [LispVal r] -> ThrowsError r (LispVal r)
numInexact2Exact [n@(Number _)] = return n
numInexact2Exact [n@(Rational _)] = return n
numInexact2Exact [(Float n)] = return $ Number $ round n
numInexact2Exact [c@(Complex _)] = numRound [c]
numInexact2Exact [badType] = throwError $ TypeMismatch "number" badType
numInexact2Exact badArgList = throwError $ NumArgs (Just 1) badArgList

-- |Convert a number to a string; radix is optional, defaults to base 10
num2String :: [LispVal r] -> ThrowsError r (LispVal r)
num2String [(Number n)] = return $ String $ show n
num2String [(Number n), (Number radix)] = do
  case radix of
    2 -> do -- Nice tip from StackOverflow question #1959715
             return $ String $ showIntAtBase 2 intToDigit n ""
    8 -> return $ String $ printf "%o" n
    10 -> return $ String $ printf "%d" n
    16 -> return $ String $ printf "%x" n
    _ -> throwError $ BadSpecialForm "Invalid radix value" $ Number radix
num2String [n@(Rational _)] = return $ String $ show n
num2String [(Float n)] = return $ String $ show n
num2String [n@(Complex _)] = return $ String $ show n
num2String [x] = throwError $ TypeMismatch "number" x
num2String badArgList = throwError $ NumArgs (Just 1) badArgList

-- | Determine if the given value is not a number
isNumNaN :: [LispVal r] -> ThrowsError r (LispVal r)
isNumNaN ([Float n]) = return $ Bool $ isNaN n
isNumNaN _ = return $ Bool False

-- | Determine if number is infinite
isNumInfinite :: [LispVal r] -> ThrowsError r (LispVal r)
isNumInfinite ([Float n]) = return $ Bool $ isInfinite n
isNumInfinite _ = return $ Bool False

-- | Determine if number is not infinite
isNumFinite :: [LispVal r] -> ThrowsError r (LispVal r)
isNumFinite ([Number _]) = return $ Bool True
isNumFinite ([Float n]) = return $ Bool $ not $ isInfinite n
isNumFinite ([Complex _]) = return $ Bool True
isNumFinite ([Rational _]) = return $ Bool True
isNumFinite _ = return $ Bool False

-- | Determine if number is exact
isNumExact :: [LispVal r] -> ThrowsError r (LispVal r)
isNumExact ([Number _]) = return $ Bool True
isNumExact ([Float _]) = return $ Bool False
isNumExact ([Complex _]) = return $ Bool False -- TODO: could be either
isNumExact ([Rational _]) = return $ Bool True
isNumExact _ = return $ Bool False

-- | Determine if number is inexact
isNumInexact :: [LispVal r] -> ThrowsError r (LispVal r)
isNumInexact ([Number _])   = return $ Bool False
isNumInexact ([Float _])    = return $ Bool True
isNumInexact ([Complex _])  = return $ Bool True
isNumInexact ([Rational _]) = return $ Bool False
isNumInexact _ = return $ Bool False

-- |Predicate to determine if given value is a number
isNumber :: [LispVal r] -> ThrowsError r (LispVal r)
isNumber ([Number _]) = return $ Bool True
isNumber ([Float _]) = return $ Bool True
isNumber ([Complex _]) = return $ Bool True
isNumber ([Rational _]) = return $ Bool True
isNumber _ = return $ Bool False

-- |Predicate to determine if given number is complex.
--  Keep in mind this does not just look at the types 
isComplex :: [LispVal r] -> ThrowsError r (LispVal r)
isComplex ([Complex _]) = return $ Bool True
isComplex ([Number _]) = return $ Bool True
isComplex ([Rational _]) = return $ Bool True
isComplex ([Float _]) = return $ Bool True
isComplex _ = return $ Bool False

-- |Predicate to determine if given number is a real.
--  Keep in mind this does not just look at the types 
isReal :: [LispVal r] -> ThrowsError r (LispVal r)
isReal ([Number _]) = return $ Bool True
isReal ([Rational _]) = return $ Bool True
isReal ([Float _]) = return $ Bool True
isReal ([Complex c]) = do
  imagPt <- numImagPart [(Complex c)]
  isExact <- isNumExact [imagPt]
  isZero <- numBoolBinopEq [imagPt, (Number 0)]
  case (isExact, isZero) of
    (Bool True, Bool True) -> return $ Bool True
    _ -> return $ Bool False
isReal _ = return $ Bool False

-- |Predicate to determine if given number is a rational.
--  Keep in mind this does not just look at the types 
isRational :: [LispVal r] -> ThrowsError r (LispVal r)
isRational ([Number _]) = return $ Bool True
isRational ([Rational _]) = return $ Bool True
isRational ([Float n]) = return $ Bool $ not $ isInfinite n 
isRational _ = return $ Bool False

-- |Predicate to determine if given number is an integer.
--  Keep in mind this does not just look at the types; 
--  a floating point input value can return true, for example.
isInteger :: [LispVal r] -> ThrowsError r (LispVal r)
isInteger ([Number _]) = return $ Bool True
isInteger ([Complex n]) = do
  return $ Bool $ (isFloatAnInteger $ Float $ realPart n) && (isFloatAnInteger $ Float $ imagPart n)
isInteger ([Rational n]) = do
    let numer = abs $ numerator n
    let denom = abs $ denominator n
    return $ Bool $ (numer >= denom) && ((mod numer denom) == 0)
isInteger ([n@(Float _)]) = return $ Bool $ isFloatAnInteger n
isInteger _ = return $ Bool False

-- |A utility function to determine if given value is a floating point
--  number representing an whole number (integer).
isFloatAnInteger :: LispVal r -> Bool
isFloatAnInteger (Float n) = 
    ((floor n) :: Integer) == ((ceiling n) :: Integer)
isFloatAnInteger _ = False

-- - end Numeric operations section ---

-- |Extract an integer from the given value, throwing a type error if
--  the wrong type is passed.
unpackNum :: LispVal r -> ThrowsError r Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
