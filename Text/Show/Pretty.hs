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

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE PatternGuards #-}
module Text.Show.Pretty
  ( -- * Generic representation of values
    Value(..), Name
  , valToStr
  , valToDoc
  , valToHtmlPage

    -- * Values using the 'Show' class
  , parseValue, reify, ppDoc, ppShow, pPrint

  , -- * Working with listlike ("foldable") collections
    ppDocList, ppShowList, pPrintList

    -- * Values using the 'PrettyVal' class
  , dumpDoc, dumpStr, dumpIO, PrettyVal(..)

    -- * Rendering values to Html
  , valToHtml, HtmlOpts(..), defaultHtmlOpts, htmlPage, Html(..)

    -- * Get location of data files
  , getDataDir

  , -- * Preprocessing of values
    PreProc(..), ppHide, ppHideNested, hideCon

    -- * Deprecated
  , ppValue
  ) where

import Data.Char(isHexDigit)
import Text.PrettyPrint
import qualified Text.Show.Parser as P
import Text.Show.Value
import Text.Show.PrettyVal
import Text.Show.Html
import Data.Foldable(Foldable,toList)
import Language.Haskell.Lexer(rmSpace,lexerPass0,Token(..))
import Paths_pretty_show (getDataDir)

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ( (<>) )
#else
import Prelude
#endif

{-# DEPRECATED ppValue "Please use `valToDoc` instead." #-}
ppValue :: Value -> Doc
ppValue = valToDoc

reify :: Show a => a -> Maybe Value
reify = parseValue . show

parseValue :: String -> Maybe Value
parseValue = P.parseValue . rmSpace . foldr joinTokens [] . lexerPass0
  where

  -- Sometimes we join tokens that are next to each other, with no spaces.
  -- This improves the printing of some malformed inputs:
  --   * Hex numbers with no 0x:    "4ab"  instead of "4 ab"
  joinTokens a@(t1,(p1,s1)) bs =
    case bs of
      (_t2,(_,s2)) : more
        | IntLit <- t1, all isHexDigit s2 -> jn IntLit
          where jn t = (t,(p1,s1++s2)) : more

      _ -> a : bs


-- | Convert a generic value into a pretty 'String', if possible.
ppShow :: Show a => a -> String
ppShow = show . ppDoc

-- | Pretty print something that may be converted to a list as a list.
-- Each entry is on a separate line, which means that we don't do clever
-- pretty printing, and so this works well for large strucutures.
ppShowList :: (Foldable f, Show a) => f a -> String
ppShowList = show . ppDocList

-- | Try to show a value, prettily. If we do not understand the value, then we
--   just use its standard 'Show' instance.
ppDoc :: Show a => a -> Doc
ppDoc a = case parseValue txt of
            Just v  -> valToDoc v
            Nothing -> text txt
  where txt = show a

-- | Pretty print something that may be converted to a list as a list.
-- Each entry is on a separate line, which means that we don't do clever
-- pretty printing, and so this works well for large strucutures.
ppDocList :: (Foldable f, Show a) => f a -> Doc
ppDocList = blockWith vcat '[' ']' . map ppDoc . toList

-- | Pretty print a generic value to stdout. This is particularly useful in the
-- GHCi interactive environment.
pPrint :: Show a => a -> IO ()
pPrint = putStrLn . ppShow

-- | Pretty print something that may be converted to a list as a list.
-- Each entry is on a separate line, which means that we don't do clever
-- pretty printing, and so this works well for large strucutures.
pPrintList :: (Foldable f, Show a) => f a -> IO ()
pPrintList = putStrLn . ppShowList

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

-- | Render a value using the 'PrettyVal' class and show it to standard out.
dumpIO :: PrettyVal a => a -> IO ()
dumpIO = putStrLn . dumpStr


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
  Date x           -> text x
  Time x           -> text x
  Quote x          -> text x


-- | This type is used to allow pre-processing of values before showing them.
data PreProc a = PreProc (Value -> Value) a

instance Show a => Show (PreProc a) where
  showsPrec p (PreProc f a) cs =
    case parseValue txt of
      Nothing -> txt ++ cs
      Just v  -> wrap (valToStr (f v))
    where
    txt    = showsPrec p a ""
    wrap t = case (t,txt) of
              (h:_,'(':_) | h /= '(' -> '(' : (t ++ ')' : cs)
              _ -> t ++ cs

-- | Hide the given constructors when showing a value.
ppHide :: (Name -> Bool) -> a -> PreProc a
ppHide p = PreProc (hideCon False p)

-- | Hide the given constructors when showing a value.
-- In addition, hide values if all of their children were hidden.
ppHideNested :: (Name -> Bool) -> a -> PreProc a
ppHideNested p = PreProc (hideCon True p)



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
ppCon "" vs = sep (map ppAtom vs)
ppCon c vs  = hang (text c) 2 (sep (map ppAtom vs))

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

block :: Char -> Char -> [Doc] -> Doc
block = blockWith sep

blockWith :: ([Doc] -> Doc) -> Char -> Char -> [Doc] -> Doc
blockWith _ a b []      = char a <> char b
blockWith f a b (d:ds)  = f $
    (char a <+> d) : [ char ',' <+> x | x <- ds ] ++ [ char b ]



