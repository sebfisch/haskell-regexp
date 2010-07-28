import System.Environment ( getArgs )
import Text.RegExp        ( fromString, acceptPartial )

main :: IO ()
main = do re:_ <- getArgs
          str <- getContents
          print $ acceptPartial (fromString re) str

{-

Failing to find a substring of the form `a.{20}a` in about 2 million
randomly generated a's and b's takes 3.5 seconds and 2 MB:

-}