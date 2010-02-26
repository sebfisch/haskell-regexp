import System.IO ( getContents )
import System.Environment ( getArgs )

import Text.RegExp

-- import Text.RegExp.Simple
-- import Text.RegExp.Parser
-- fromString = parse

main = do r:_ <- getArgs
          s   <- getContents
          print (acceptSubword (fromString r) s)
