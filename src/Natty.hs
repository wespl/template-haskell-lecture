{-# LANGUAGE TemplateHaskell #-}
module Natty where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data Nat = Z | S Nat
  deriving (Show, Eq, Ord)

toInt :: Nat -> Int
toInt Z = 0
toInt (S n) = 1 + toInt n

fromInt :: Int -> Nat
fromInt 0 = Z
fromInt n = if n < 0 then Z else S (fromInt (n - 1))

-- Metaprogramming

reflectNat :: Nat -> Exp
reflectNat Z = ConE 'Z
reflectNat (S n) = AppE (ConE 'S) (reflectNat n)

singleNat :: Int -> Q [Dec]
singleNat i = do
    let nat = returnQ (reflectNat (fromInt i))
    xs <- [d|
            n :: Nat
            n = $nat
          |]
    return $ map (rename (mkName ("n" ++ show i))) xs
  where
    rename :: Name -> Dec -> Dec
    rename new (SigD _ t) = SigD new t
    rename new (FunD _ cla) = FunD new cla
    rename new (ValD (VarP _) body decs) = ValD (VarP new) body decs
    rename _ d = d

makeNats :: [Int] -> Q [Dec]
makeNats xs = concat <$> mapM singleNat xs

-- Generate tuple selector

selectors :: Int -> [(Int, Int)]
selectors bound =
  [(i, n) | i <- [1..bound], n <- [1..bound], i <= n]

singleProjection :: (Int, Int) -> Q Dec
singleProjection (nth, outOf) = do
  names <- replicateM outOf (newName "a")
  let nthName = names !! (nth - 1)
  return $
    FunD (mkName $ "proj" ++ show nth ++ "of" ++ show outOf)
         [Clause [TupP (map VarP names)]
                 (NormalB (VarE nthName)) []]

makeProjections :: Int -> Q [Dec]
makeProjections bound = mapM singleProjection (selectors bound)
