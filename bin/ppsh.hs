import Text.Show.Pretty
import Data.Either(partitionEithers)
import Data.List(isPrefixOf)
import Data.Char(isSpace)
import System.Environment(getArgs)
import System.IO(hPutStrLn,stderr)

main :: IO ()
main =
  do as <- getArgs
     let (opts,hiding) = partitionEithers (map isHiding as)
         preproc v = case hiding of
                       [] -> v
                       _  -> hideCon False (`elem` hiding) v
     case opts of
       ["--test"] -> interactLn (show . selftest1)

       ["--html"] ->
         do txt <- getContents
            case parseValue txt of
              Just v  ->
                do dir <- getDataDir
                   let optis = defaultHtmlOpts { dataDir = dir }
                   -- XXX: perhaps for HTML the "hidden" values should
                   -- just start off collapsed, rahter than being deleted?
                   putStrLn (valToHtmlPage optis (preproc v))
              Nothing -> hPutStrLn stderr "Failed to parse value."

       []         -> interactLn $ \s -> case parseValue s of
                                          Just v  -> show (valToDoc (preproc v))
                                          Nothing -> s
       _ -> hPutStrLn stderr $ unlines
              [ "usage: ppsh < showed_value > pretty_value"
              , "   --hide=CON  Hide this constructor."
              , "   --html      Generate HTML."
              , "   --test      Self test: True means we passed."
              ]
  where
  isHiding a
    | pref `isPrefixOf` a = Right $ dropWhile isSpace
                                  $ drop (length pref) a
    | otherwise = Left a
    where pref = "--hide="


interactLn :: (String -> String) -> IO ()
interactLn f = interact f >> putStrLn ""

selftest :: Value -> Bool
selftest v = case parseValue $ show $ valToDoc v of
               Just v1  -> v1 == v
               Nothing  -> False

selftest1 :: String -> Bool
selftest1 txt = case parseValue txt of
                  Just v  -> selftest v
                  Nothing -> True



