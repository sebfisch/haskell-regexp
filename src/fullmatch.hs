import System.Environment ( getArgs )
import Text.RegExp        ( fromString, (=~) )

main :: IO ()
main = do re:_ <- getArgs
          str <- getContents
          print $ fromString re =~ str

{-

Matching 100 MB of a's against .* takes 6 seconds (that is 16 MB/s)
and runs in constant space (1 MB).

Matching 5000 a's against (a?){5000}a{5000} takes 12 seconds and runs
in 5 MB. 9 seconds are spent during garbage collection.

Matching about 2 million randomly generated a's and b's that do not
match .*a.{20}a.* against this expression takes 3.5 seconds and 2 MB.

-}