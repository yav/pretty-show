module Text.Show.Html
  ( HtmlOpts(..), defaultHtmlOpts
  , dumpHtml, toHtml, htmlPage
  ) where

import Text.Show.Value
import Prelude hiding (span)

dumpHtml :: HtmlOpts -> Value -> Html
dumpHtml opts = htmlPage opts . toHtml opts

data HtmlOpts = HtmlOpts
  { dataDir :: FilePath
  }

defaultHtmlOpts :: HtmlOpts
defaultHtmlOpts = HtmlOpts
  { dataDir = ""
  }


toHtml :: HtmlOpts -> Value -> Html
toHtml opts val =
  case val of
    Con con []  -> span "con" (text con)
    Con con vs  -> tallRecord con (map conLab vs) (map (toHtml opts) vs)
    Rec con fs  -> tallRecord con (map fst fs) (map (toHtml opts . snd) fs)
    Tuple vs    -> wideTuple (map (toHtml opts) vs)

    List []     -> span "list" (text "[]")
    List vs@(v : vs1) ->
      case v of

        Con c fs
          | all (isCon c) vs1  -> recordList c (map conLab fs)
                                   [ map (toHtml opts) xs | Con _ xs <- vs ]
          | otherwise          -> tallList $ map (toHtml opts) vs

        Rec c fs
          | all (isRec c) vs1   -> recordList c (map fst fs)
                                 [ map (toHtml opts . snd) xs | Rec _ xs <- vs ]
          | otherwise           -> tallList $ map (toHtml opts) vs

        Tuple fs -> tupleList (length fs)
                          [ map (toHtml opts) xs | Tuple xs <- vs ]

        List {}    -> tallList    $ map (toHtml opts) vs

        Neg {}     -> wideList 80 $ map (toHtml opts) vs
        Ratio {}   -> wideList 80 $ map (toHtml opts) vs
        Integer {} -> wideList 80 $ map (toHtml opts) vs
        Float {}   -> wideList 80 $ map (toHtml opts) vs
        Char {}    -> wideList 80 $ map (toHtml opts) vs
        String {}  -> tallList    $ map (toHtml opts) vs

    Neg v       ->
      case v of
        Integer txt -> span "integer" ('-' : txt)
        Float txt   -> span "float"   ('-' : txt)
        _           -> neg (toHtml opts v)

    Ratio v1 v2 -> ratio (toHtml opts v1) (toHtml opts v2)
    Integer txt -> span "integer" (text txt)
    Float txt   -> span "float"   (text txt)
    Char txt    -> span "char"    (text txt)
    String txt  -> span "string"  (text txt)

  where
  conLab _          = " "

  isCon c (Con d _) = c == d
  isCon _ _         = False

  isRec c (Rec d _) = c == d
  isRec _ _         = False


neg :: Html -> Html
neg e = table "negate" [ tr [td (text "-"), td e] ]

ratio :: Html -> Html -> Html
ratio e1 e2 = table "ratio" [ tr [ td' "numerator" e1 ], tr [td e2] ]

wideTuple :: [Html] -> Html
wideTuple els = table "wideTuple" [ tr $ map td els ]

tallTuple :: [Html] -> Html
tallTuple els = table "tallTuple" $ map (tr . return . td) els

tallRecord :: Name -> [Name] -> [Html] -> Html
tallRecord con labs els = table "tallRecord" $ topHs : zipWith row labs els
  where
  topHs   = tr [ th "con" 2 (text con) ]
  row l e = tr [ th "label" 1 (text l),   td e ]

recordList :: Name -> [Name] -> [[Html]] -> Html
recordList con labs els = table "recordList" $ topHs : zipWith row [0..] els
  where
  topHs    = tr $ th "con" 1 (text con) : map (th "label" 1 . text) labs
  row n es = tr $ th "ix" 1 (int n) : map td es

tupleList :: Int -> [[Html]] -> Html
tupleList n els = recordList " " (replicate n " ") els

tallList :: [Html] -> Html
tallList els = table "tallList" $ top : zipWith row [0..] els
  where
  top     = tr [ th "con" 2 (text " ")]
  row n e = tr [ th "ix" 1 (int n), td e ]

wideList :: Int -> [Html] -> Html
wideList w els = table "wideList" $ topHs : zipWith row [0..] (chop els)
  where
  elNum = length els
  pad   = elNum > w

  chop [] = []
  chop xs = let (as,bs) = splitAt w xs
            in take w (as ++ if pad then repeat empty else []) : chop bs

  topHs     = tr $ th "con" 1 (text " ") : map (th "label" 1 . int)
                                                [ 0 .. min elNum w - 1 ]
  row n es  = tr $ (th "ix" 1 (int (n*w))) : map td es

--------------------------------------------------------------------------------
type Html = String

table :: String -> [Html] -> Html
table cl body = "<table class=" ++ show cl ++ ">" ++ concat body ++ "</table>"

tr :: [Html] -> Html
tr body = "<tr>" ++ concat body ++ "</tr>"

th :: String -> Int -> Html -> Html
th cl n body = "<th class=" ++ show cl ++ " colspan=" ++ show (show n) ++ ">"
                                          ++ body ++ "</th>"

td :: Html -> Html
td body = "<td>" ++ body ++ "</td>"

td' :: String -> Html -> Html
td' cl body = "<td class=" ++ show cl ++ ">" ++ body ++ "</td>"



span :: String -> Html -> Html
span cl body = "<span class=" ++ show cl ++ ">" ++ body ++ "</span>"

empty :: Html
empty = ""

int :: Int -> Html
int = show

text :: String -> Html
text = concatMap esc
  where
  esc '<' = "&lt;"
  esc '>' = "&gt;"
  esc '&' = "&amp;"
  esc ' ' = "&nbsp;"
  esc c   = [c]

htmlPage :: HtmlOpts -> Html -> Html
htmlPage opts body =
  unlines
  [ "<html>"
  , "<head>"
  , "<link href="  ++ show pstyle ++ " rel=" ++ show "stylesheet" ++ ">"
  , "<script src=" ++ show jquery ++ "></script>"
  , "<script src=" ++ show pjs    ++ "></script>"
  , "<body>"
  , body
  , "</body>"
  , "</html>"
  ]
  where
  dir    = dataDir opts
  -- XXX: slashes on Windows?
  jquery = dir ++ "/style/jquery.js"
  pjs    = dir ++ "/style/pretty-show.js"
  pstyle = dir ++ "/style/pretty-show.css"


