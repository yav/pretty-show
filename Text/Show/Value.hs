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

-- | Generic Haskell values.
-- 'NaN' and 'Infinity' are represented as constructors.
-- The 'String' in the literals is the text for the literals \"as is\".
data Value    = Con Name [Value]            -- ^ Data constructor
              | Rec Name [ (Name,Value) ]   -- ^ Record value
              | Tuple [Value]               -- ^ Tuple
              | List [Value]                -- ^ List
              | Neg Value                   -- ^ Negated value
              | Ratio Value Value           -- ^ Rational
              | Integer String              -- ^ Non-negative integer
              | Float String                -- ^ Non-negative floating num.
              | Char String                 -- ^ Non-negative character
              | String String               -- ^ String
                deriving (Eq,Show)
