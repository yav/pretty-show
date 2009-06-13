--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Value
-- Copyright   :  (c) Iavor S. Diatchki 2009
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
-- Portability :  Haskell 98
--
-- Generic representation of Showable values.
--------------------------------------------------------------------------------


module Text.Show.Value ( Name, Value(..) ) where

-- | A name.
type Name     = String

-- | Generic Haskell values
data Value    = Con Name [Value]            -- ^ Data constructor
              | Rec Name [ (Name,Value) ]   -- ^ Record value
              | Tuple [Value]               -- ^ Tuple
              | List [Value]                -- ^ List
              | Other String                -- ^ Something else (e.g. number)
                deriving (Eq,Show)
