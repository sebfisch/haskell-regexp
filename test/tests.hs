-- import Text.RegExp
-- import Text.RegExp.Data
-- import Text.RegExp.Matcher

import Text.RegExp.Simple

import System        ( getArgs )
import System.Random ( randomRIO )

evilRegExp n = bounded (optional (char 'a')) (n,n) .*. bounded (char 'a') (n,n)

regexp n = char 'a' .*. bounded (symbol "." (const True)) (n,n) .*. char 'a'

aNbN = epsilon .+. (char 'a' .*. aNbN .*. char 'b')
 where
  r .*. s = RegExp (isEmpty r && isEmpty s) Nothing (r:*:s)
  r .+. s = RegExp (isEmpty r || isEmpty s) Nothing (r:+:s)

aNbNcN = epsilon .+. abc 1
 where
  abc n   = char 'a' .*. (pow 'b' n .*. pow 'c' n .+. abc (n+1))
  pow a n = foldr (.*.) epsilon (replicate n (char a))

  r .*. s = RegExp (isEmpty r && isEmpty s) Nothing (r:*:s)
  r .+. s = RegExp (isEmpty r || isEmpty s) Nothing (r:+:s)

main = do n <- (read.head) `fmap` getArgs

--           s <- head `fmap` getArgs
--           print $ accept aNbNcN s

--           s <- randomAB (n*n)
--           putStrLn s
--           mapM_ print $ process (regexp n) (zip (repeat (Any True)) s)
--           print $ accept (regexp n) s

--           mapM_ print $ process (evilRegExp n) (replicate (2*n) 'a')
          print $ accept (evilRegExp n) (replicate (2*n) 'a')

randomAB :: Int -> IO String
randomAB 0 = return ""
randomAB n = do c <- randomRIO ('a','b')
                s <- randomAB (n-1)
                return (c:s)

-- crashes for n > 357
-- sedTest n = do s <- randomAB (n*n) 
--                system $ "echo \""++s++"\" | sed s/"
--                      ++ "\\(a\\|b\\)*a" ++ concat (replicate n "\\(a\\|b\\)")
--                      ++ "a\\(a\\|b\\)*/x/ > /dev/null"
