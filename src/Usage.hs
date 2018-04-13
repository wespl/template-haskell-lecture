{-# LANGUAGE TemplateHaskell #-}
module Usage where

import Natty

$(makeNats [1..10])

$(makeProjections 5)
