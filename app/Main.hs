{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Z3.Monad as Z
import Control.Monad 
import Control.Arrow
import Data.Maybe

data Solution = Solution
    { a :: Int
    , b :: Int
    , c :: Bool
    , d :: Int
    , e :: Bool
    , f :: Int
    , g :: Bool
    , h :: Int
    , i :: Bool
    , j :: Either Bool Int
    } deriving (Show)


data Z3Solution = Z3Solution
    { a :: Z.AST
    , b :: Z.AST 
    , c :: Z.AST 
    , d :: Z.AST 
    , e :: Z.AST 
    , f :: Z.AST 
    , g :: Z.AST 
    , h :: Z.AST 
    , i :: Z.AST 
    , jisbool :: Bool
    , jint :: Z.AST 
    , jbool :: Z.AST 
    } deriving (Show)

mkZ3Solution :: Bool -> Z.Z3 Z3Solution
mkZ3Solution jisbool = Z3Solution
    <$> Z.mkFreshIntVar "a"
    <*> Z.mkFreshIntVar "b"
    <*> Z.mkFreshBoolVar "c"
    <*> Z.mkFreshIntVar "d"
    <*> Z.mkFreshBoolVar "e"
    <*> Z.mkFreshIntVar "f"
    <*> Z.mkFreshBoolVar "g"
    <*> Z.mkFreshIntVar "h"
    <*> Z.mkFreshBoolVar "i"
    <*> pure jisbool
    <*> Z.mkFreshIntVar "jint"
    <*> Z.mkFreshBoolVar "jbool"

getAllBoolsZ3 :: Z3Solution -> [Z.AST]
getAllBoolsZ3 Z3Solution {..} = let bools = [c,e,g,i] in if jisbool then jbool:bools else bools

getAllIntsZ3 :: Z3Solution -> [Z.AST]
getAllIntsZ3 Z3Solution {..} = let ints = [a,b,d,f,h] in if jisbool then ints else jint:ints

(<$$>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
f <$$> arg = fmap (fmap f) arg

(<**>) :: (Monad a1, Monad a2) => a1 (a2 (a -> b)) -> a1 (a2 a) -> a1 (a2 b)
f <**> arg = f >>= \f -> arg >>= \arg -> pure (ap f arg)


evalInt' :: Z.Model -> Z.AST -> Z.Z3 (Maybe Int)
evalInt' m v = fromIntegral <$$> Z.evalInt m v

mkIntDiv :: Z.AST -> Z.AST -> Z.Z3 Z.AST
mkIntDiv lhs rhs = do
    -- Assert that the divider is not zero
    Z.assert =<< Z.mkNot =<< Z.mkEq rhs =<< Z.mkInteger 0
    -- Assert that lhs % rhs == 0
    Z.assert =<< uncurry Z.mkEq =<< (,) <$> Z.mkMod lhs rhs <*> Z.mkInteger 0
    -- Proceed with the division
    Z.mkDiv lhs rhs


boolToInt :: Z.AST -> Z.Z3 Z.AST
boolToInt v = do
    _0 <- Z.mkInteger 0
    _1 <- Z.mkInteger 1
    Z.mkIte v _1 _0



solve :: Z.Z3 (Maybe Solution)
solve = do
    sol@Z3Solution {..} <- mkZ3Solution False
    _0 <- Z.mkInteger 0
    _1 <- Z.mkInteger 1

    let ints = getAllIntsZ3 sol
    let numints = length ints
    let bools = getAllBoolsZ3 sol
    boolints <- mapM boolToInt bools

    -- a is the sum of ints
    Z.assert =<< Z.mkEq a =<< Z.mkAdd ints

    -- b is the number of trues
    Z.assert =<< Z.mkEq b =<< Z.mkAdd boolints

    -- c is wether a is the largest
    Z.assert =<< Z.mkEq c =<< Z.mkAnd =<< mapM (Z.mkGe a) ints

    -- d is how many are equal to me
    Z.assert =<< Z.mkEq d =<< Z.mkAdd =<< mapM (Z.mkEq d >=> boolToInt) ints

    -- e is whether all integers are positive
    Z.assert =<< Z.mkEq e =<< Z.mkAnd =<< mapM (Z.mkLe _0) ints

    -- f is the average of all ints
    Z.assert =<< Z.mkEq f =<< mkIntDiv a =<< Z.mkInteger (fromIntegral numints)

    -- g is d>b
    Z.assert =<< Z.mkEq g =<< Z.mkGt d b

    -- h is a/h
    Z.assert =<< Z.mkEq h =<< mkIntDiv a h

    -- i is f == d - b - h * d
    htimesd <- Z.mkMul [h,d]
    Z.assert =<< Z.mkEq i =<< Z.mkEq f =<< Z.mkSub [d,b,htimesd]


    -- eval the values
    fmap (fmap fromJust . snd) $ Z.withModel $ \m -> Solution 
        <$$> evalInt' m a
        <**> evalInt' m b
        <**> Z.evalBool m c
        <**> evalInt' m d
        <**> Z.evalBool m e
        <**> evalInt' m f
        <**> Z.evalBool m g
        <**> evalInt' m h
        <**> Z.evalBool m i
        <**> if jisbool then Left <$$> Z.evalBool m jbool else Right <$$> evalInt' m jint

main :: IO ()
main = Z.evalZ3 solve >>= print
