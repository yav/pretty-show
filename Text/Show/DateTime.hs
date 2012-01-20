-- | This module provides some support for recognizing various date
-- formats used by the 'time' package.  Clearly, this is a bit of a hack
-- but we try to be helpful.
module Text.Show.DateTime (parseDateTime) where

import Language.Haskell.Lexer(Token(..), PosToken, lexerPass0)

parseDateTime :: [PosToken] -> [PosToken]
parseDateTime inp =
  case date inp of
    Just (x@(tok,(p,d)), (y@(Whitespace,(_," ")) : rest)) ->
      case time rest of
        Just ((_,(_,t)), rest1) -> (tok, (p, d ++ t)) : parseDateTime rest1
        Nothing -> x : y : parseDateTime rest

    Just (x, rest) -> x : parseDateTime rest
    Nothing ->
      case time inp of
        Just (x, rest) -> x : parseDateTime rest
        Nothing ->
          case timeDiff inp of
            Just (x, rest) -> x : parseDateTime rest
            Nothing ->
              case inp of
                x : xs -> x : parseDateTime xs
                []     -> []
  where
  date ( (IntLit,(p,y)) :
         (Varsym,(_,"-")) :
         (IntLit,(_,m)) :
         (Varsym,(_,"-")) :
         (IntLit,(_,d)) : rest
       ) = Just ((StringLit, (p, y ++ "-" ++ m ++ "-" ++ d)), rest)

  date _ = Nothing

  time ( (IntLit,(p,h)) :
         (Reservedop,(_,":")) :
         (IntLit,(_,m)) :
         (Reservedop,(_,":")) :
         (mbSecs,(_,s)) :
         rest
       ) | mbSecs == IntLit || mbSecs == FloatLit =
            Just ((StringLit, (p,h ++ ":" ++ m ++ ":" ++ s ++ tz)), rest1)
         where (tz,rest1) = timezone rest
  time _ = Nothing

  timezone ((Whitespace,(_," ")) : (Conid,(_,c)) : rest) = (" " ++ c, rest)
  timezone xs = ("", xs)

  timeDiff ((mbLen,(p,h)) : (Varid,(_,"s")) : rest)
    | mbLen == IntLit || mbLen == FloatLit =
                            Just ((StringLit, (p, h ++ "s")), rest)
  timeDiff _ = Nothing


test x = mapM_ print $ parseDateTime $ lexerPass0 x
