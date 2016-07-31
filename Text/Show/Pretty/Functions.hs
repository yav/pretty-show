{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Show.Pretty.Functions where

instance Show (a -> b) where
  showsPrec _ _ = showString "_fn"
