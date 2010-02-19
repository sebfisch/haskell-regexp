import Text.RegExp

-- import Text.RegExp.Simple
-- import Text.RegExp.Parser
-- fromString = parse

import Control.Monad ( when )

import System.IO ( getContents )

main = do ls <- fmap lines getContents
          mapM_ check $ triples ls

triples ("regexp:":r:"matches:":ls) = (r,ts,fs) : triples ls2
 where
  (ts,_:ls1) = break ("does not match:"==) ls
  (fs,ls2)   = break (`elem`["regexp:","end of tests"]) ls1

triples _ = []

check (r,[],[])   = return ()
check (r,t:ts,fs) = do when (not (accept (fromString r) t))
                            (putStr (errorMsg r t True))
                       check (r,ts,fs)
check (r,[],f:fs) = do when (accept (fromString r) f)
                            (putStr (errorMsg r f False))
                       check (r,[],fs)

errorMsg r s b = unlines ["Failing test:"
                         ,r
                         ,"should " ++ (if b then "" else "not ") ++ "match"
                         ,s]

