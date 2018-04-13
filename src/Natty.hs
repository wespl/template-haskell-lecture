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

makeNats :: [Int] -> Q [Dec]
makeNats xs = undefined

-- Generate tuple selector

makeProjections :: Int -> Q [Dec]
makeProjections bound = undefined
