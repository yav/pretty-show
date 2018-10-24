--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Value
-- Copyright   :  (c) Iavor S. Diatchki 2009
-- License     :  MIT
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
-- Portability :  Haskell 98
--
-- Generic representation of Showable values.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
module Text.Show.Value ( Name, Value(..), hideCon ) where

import Data.Maybe(fromMaybe,isNothing)

-- | A name.
type Name     = String

-- | Generic Haskell values.
-- 'NaN' and 'Infinity' are represented as constructors.
-- The 'String' in the literals is the text for the literals \"as is\".
--
-- A chain of infix constructors means that they appeared in the input string
-- without parentheses, i.e
--
-- @1 :+: 2 :*: 3@ is represented with @InfixCons 1 [(":+:",2),(":*:",3)]@, whereas
--
-- @1 :+: (2 :*: 3)@ is represented with @InfixCons 1 [(":+:",InfixCons 2 [(":*:",3)])]@.
data Value    = Con Name [Value]               -- ^ Data constructor
              | InfixCons Value [(Name,Value)] -- ^ Infix data constructor chain
              | Rec Name [ (Name,Value) ]      -- ^ Record value
              | Tuple [Value]                  -- ^ Tuple
              | List [Value]                   -- ^ List
              | Neg Value                      -- ^ Negated value
              | Ratio Value Value              -- ^ Rational
              | Integer String                 -- ^ Non-negative integer
              | Float String                   -- ^ Non-negative floating num.
              | Char String                    -- ^ Character
              | String String                  -- ^ String
                deriving (Eq,Show)

{- | Hide constrcutros matching the given predicate.
If all fields of a constructor are hidden, then we also hide
the constructor itslef.  If a record field is hidden,
then we do not show its label either. -}
hideCon :: (Name -> Bool) -> Value -> Value
hideCon hidden = toVal . delMaybe
  where
  hiddenV = Con "" []

  toVal = fromMaybe hiddenV

  delMany vals
    | all isNothing newVals = Nothing
    | otherwise = Just (map toVal newVals)
    where
    newVals = map delMaybe vals

  delMaybe val =
    case val of
      Con x vs
        | hidden x  -> Nothing
        | null vs   -> Just val
        | otherwise -> Con x <$> delMany vs

      Rec x fs
        | hidden x  -> Nothing
        | null fs   -> Just val
        | all isNothing mbs -> Nothing
        | otherwise -> Just (Rec x [ (f,v) | (f,Just v) <- zip ls mbs ])
        where (ls,vs) = unzip fs
              mbs     = map delMaybe vs

      InfixCons v ys
        | any hidden cs -> Nothing
        | otherwise -> do ~(v1:vs1) <- delMany (v:vs)
                          Just (InfixCons v1 (zip cs vs1))
          where (cs,vs) = unzip ys

      Tuple vs | null vs   -> Just val
               | otherwise -> Tuple <$> delMany vs
      List vs  | null vs -> Just val
               | otherwise -> List  <$> delMany vs
      Neg v       -> Neg   <$> delMaybe v
      Ratio v1 v2 -> do ~[a,b] <- delMany [v1,v2]
                        Just (Ratio a b)
      Integer {}  -> Just val
      Float {}    -> Just val
      Char {}     -> Just val
      String {}   -> Just val


