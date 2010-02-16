import Text.RegExp

import System.IO ( getContents )
import System.Environment ( getArgs )

main = do r:_ <- getArgs
          s   <- getContents
          print (s =~ fromString r)
