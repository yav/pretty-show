--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Pretty
-- Copyright   :  (c) Iavor S. Diatchki 2009
-- License     :  MIT
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
-- Portability :  Haskell 98
--
-- Functions for human-readable derived 'Show' instances.
--------------------------------------------------------------------------------

{-# LANGUAGE Safe #-}
module Text.Show.Pretty
  ( -- * Generic representation of values
    Value(..), Name
  , valToStr
  , valToDoc
  , valToHtmlPage

    -- * Values using the 'Show' class
  , parseValue, reify, ppDoc, ppShow

    -- * Values using the 'PrettyVal' class
  , dumpDoc, dumpStr, PrettyVal(..)

    -- * Rendering values to Html
  , valToHtml, HtmlOpts(..), defaultHtmlOpts, htmlPage, Html(..)

    -- * Get location of data files
  , getDataDir

    -- * Deprecated
  , ppValue
  ) where

import Text.PrettyPrint
import qualified Text.Show.Parser as P
import Text.Show.Value
import Text.Show.PrettyVal
import Text.Show.Html
import Language.Haskell.Lexer(rmSpace,lexerPass0)
import Paths_pretty_show (getDataDir)

{-# DEPRECATED ppValue "Please use `valToDoc` instead." #-}
ppValue :: Value -> Doc
ppValue = valToDoc

reify :: Show a => a -> Maybe Value
reify = parseValue . show

parseValue :: String -> Maybe Value
parseValue = P.parseValue . rmSpace . lexerPass0

-- | Convert a generic value into a pretty 'String', if possible.
ppShow :: Show a => a -> String
ppShow = show . ppDoc

-- | Try to show a value, prettily. If we do not understand the value, then we
--   just use its standard 'Show' instance.
ppDoc :: Show a => a -> Doc
ppDoc a = case parseValue txt of
            Just v  -> valToDoc v
            Nothing -> text txt
  where txt = show a

-- | Render a value in the 'PrettyVal' class to a 'Doc'.
-- The benefit of this function is that 'PrettyVal' instances may
-- be derived automatically using generics.
dumpDoc :: PrettyVal a => a -> Doc
dumpDoc = valToDoc . prettyVal

-- | Render a value in the 'PrettyVal' class to a 'String'.
-- The benefit of this function is that 'PrettyVal' instances may
-- be derived automatically using generics.
dumpStr :: PrettyVal a => a -> String
dumpStr = show . dumpDoc


-- | Pretty print a generic value. Our intention is that the result is
--   equivalent to the 'Show' instance for the original value, except possibly
--   easier to understand by a human.
valToStr :: Value -> String
valToStr = show . valToDoc

-- | Pretty print a generic value. Our intention is that the result is
--   equivalent to the 'Show' instance for the original value, except possibly
--   easier to understand by a human.
valToDoc :: Value -> Doc
valToDoc val = case val of
  Con c vs         -> ppCon c vs
  InfixCons v1 cvs -> hang_sep (go v1 cvs)
    where
      go v []            = [ppInfixAtom v]
      go v ((n,v2):cvs') = (ppInfixAtom v <+> text n):go v2 cvs'

      hang_sep [] = empty
      hang_sep (x:xs) = hang x 2 (sep xs)
    -- hang (ppInfixAtom v1) 2 (sep [ text n <+> ppInfixAtom v | (n,v) <- cvs ])
  Rec c fs         -> hang (text c) 2 $ block '{' '}' (map ppField fs)
    where ppField (x,v) = hang (text x <+> char '=') 2 (valToDoc v)

  List vs          -> block '[' ']' (map valToDoc vs)
  Tuple vs         -> block '(' ')' (map valToDoc vs)
  Neg v            -> char '-' <> ppAtom v
  Ratio x y        -> hang (ppAtom x <+> text "%") 2 (ppAtom y)
  Integer x        -> text x
  Float x          -> text x
  Char x           -> text x
  String x         -> text x


-- Private ---------------------------------------------------------------------

ppAtom :: Value -> Doc
ppAtom v
  | isAtom v  = valToDoc v
  | otherwise = parens (valToDoc v)

ppInfixAtom :: Value -> Doc
ppInfixAtom v
  | isInfixAtom v = valToDoc v
  | otherwise     = parens (valToDoc v)

ppCon :: Name -> [Value] -> Doc
ppCon c vs = hang (text c) 2 (sep (map ppAtom vs))

isAtom               :: Value -> Bool
isAtom (Con _ (_:_))  = False
isAtom (InfixCons {}) = False
isAtom (Ratio {})     = False
isAtom (Neg {})       = False
isAtom _              = True

-- Don't put parenthesis around constructors in infix chains
isInfixAtom          :: Value -> Bool
isInfixAtom (InfixCons {}) = False
isInfixAtom (Ratio {})     = False
isInfixAtom (Neg {})       = False
isInfixAtom _              = True

block            :: Char -> Char -> [Doc] -> Doc
block a b []      = char a <> char b
block a b (d:ds)  = sep $
    (char a <+> d) : [ char ',' <+> x | x <- ds ] ++ [ char b ]

