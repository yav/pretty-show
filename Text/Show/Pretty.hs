--------------------------------------------------------------------------------
-- |
-- Module      :  Text.Show.Pretty
-- Copyright   :  (c) Iavor S. Diatchki 2009
-- License     :  BSD3
--
-- Maintainer  :  iavor.diatchki@gmail.com
-- Stability   :  provisional
-- Portability :  Haskell 98
--
-- Functions for human-readable derived 'Show' instances.
--------------------------------------------------------------------------------


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
  Con c vs    -> ppCon c vs
  Rec c fs    -> hang (text c) 2 $ block '{' '}' (map ppField fs)
    where ppField (x,v) = text x <+> char '=' <+> valToDoc v

  List vs     -> block '[' ']' (map valToDoc vs)
  Tuple vs    -> block '(' ')' (map valToDoc vs)
  Neg v       -> char '-' <> ppAtom v
  Ratio x y   -> ppCon "(%)" [x,y]
  Integer x   -> text x
  Float x     -> text x
  Char x      -> text x
  String x    -> text x


-- Private ---------------------------------------------------------------------

ppAtom :: Value -> Doc
ppAtom v
  | isAtom v  = valToDoc v
  | otherwise = parens (valToDoc v)

ppCon :: Name -> [Value] -> Doc
ppCon c []        = text c
ppCon c (v : vs)  = hang line1 2 (foldl addParam doc1 vs)
  where (line1,doc1)
          | isAtom v   = (text c, valToDoc v)
          | otherwise  = (text c <+> char '(', valToDoc v <+> char ')')

        addParam d p
          | isAtom p  = d $$ valToDoc p
          | otherwise = (d <+> char '(') $$ (valToDoc p <+> char ')')

isAtom               :: Value -> Bool
isAtom (Con _ (_:_))  = False
isAtom (Ratio {})     = False
isAtom (Neg {})       = False
isAtom _              = True

block            :: Char -> Char -> [Doc] -> Doc
block a b []      = char a <> char b
block a b (d:ds)  = char a <+> d
                 $$ vcat [ char ',' <+> x | x <- ds ]
                 $$ char b

