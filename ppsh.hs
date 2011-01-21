import Text.Show.Pretty
import System.Environment
import System.IO(hPutStrLn,stderr)

main :: IO ()
main =
  do as <- getArgs
     case as of
       ["--test"] -> interactLn (show . selftest1)

       ["--html"] ->
         do txt <- getContents
            case parseValue txt of
              Just v  -> putStrLn (html v)
              Nothing -> hPutStrLn stderr "Failed to parse value."

       []         -> interactLn $ \s -> case parseValue s of
                                          Just v  -> show (ppValue v)
                                          Nothing -> s
       _ -> hPutStrLn stderr $ unlines
              [ "usage: ppsh < showed_value > pretty_value"
              , "   --html      Generate HTML."
              , "   --test      Self test: True means we passed."
              ]


interactLn :: (String -> String) -> IO ()
interactLn f = interact f >> putStrLn ""

selftest :: Value -> Bool
selftest v = case parseValue $ show $ ppValue v of
               Just v1  -> v1 == v
               Nothing  -> False

selftest1 :: String -> Bool
selftest1 txt = case parseValue txt of
                  Just v  -> selftest v
                  Nothing -> True

html :: Value -> String
html val =
  case val of
    Con n xs  -> node "con" (name n : map html xs)
    Rec n xs  -> node "rec" (name n : map field xs)
    Tuple xs  -> node "tuple" (map html xs)
    List xs   -> node "list" (map html xs)
    Neg x     -> node "neg" [ html x ]
    Ratio x y -> node "ratio" [ html x, html y ]
    Integer x -> leaf "integer" x
    Float x   -> leaf "float" x
    Char x    -> leaf "char" x
    String x  -> leaf "string" x

  where
  name        = leaf "name"
  field (x,v) = node "field" [ name x, html v ]

  leaf c txt  = "<div class=\"" ++ c ++ "\">" ++ concatMap esc txt ++ "</div>"
  node c els  = "<div class=\"" ++ c ++ "\">" ++ concat els ++ "</div>"
  esc c =
    case c of
      '<' -> "&lt;"
      '>' -> "&gt;"
      '&' -> "&amp;"
      '\''-> "&apos;"
      '"' -> "&quot;"
      _   -> [c]

