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
  ( Name, Value(..)
  , parseValue, ppValue, ppDoc, ppShow
  ) where

import Text.PrettyPrint
import qualified Text.Show.Parser as P
import Text.Show.Value
import Language.Haskell.Lexer(rmSpace,lexerPass0)

parseValue :: String -> Maybe Value
parseValue = P.parseValue . rmSpace . lexerPass0

-- | Convert a generic value into a pretty 'String', if possible.
ppShow :: Show a => a -> String
ppShow = show . ppDoc

-- | Try to show a value, prettily. If we do not understand the value, then we
--   just use its standard 'Show' instance.
ppDoc :: Show a => a -> Doc
ppDoc a = case parseValue txt of
            Just v  -> ppValue v
            Nothing -> text txt
  where txt = show a


-- | Pretty print a generic value. Our intention is that the result is
--   equivalent to the 'Show' instance for the original value, except possibly
--   easier to understand by a human.
ppValue :: Value -> Doc
ppValue val = case val of
  Con c vs    -> ppCon c vs
  Rec c fs    -> hang (text c) 2 $ block '{' '}' (map ppField fs)
    where ppField (x,v) = text x <+> char '=' <+> ppValue v

  List vs     -> block '[' ']' (map ppValue vs)
  Tuple vs    -> block '(' ')' (map ppValue vs)
  Neg v       -> char '-' <> ppAtom v
  Ratio x y   -> ppCon "(%)" [x,y]
  Integer x   -> text x
  Float x     -> text x
  Char x      -> text x
  String x    -> text x


-- Private ---------------------------------------------------------------------

ppAtom :: Value -> Doc
ppAtom v
  | isAtom v  = ppValue v
  | otherwise = parens (ppValue v)

ppCon :: Name -> [Value] -> Doc
ppCon c []        = text c
ppCon c (v : vs)  = hang line1 2 (foldl addParam doc1 vs)
  where (line1,doc1)
          | isAtom v   = (text c, ppValue v)
          | otherwise  = (text c <+> char '(', ppValue v <+> char ')')

        addParam d p
          | isAtom p  = d $$ ppValue p
          | otherwise = (d <+> char '(') $$ (ppValue p <+> char ')')

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

