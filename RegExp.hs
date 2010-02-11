{-# LANGUAGE FlexibleInstances #-}

import Prelude hiding ( flip, last )

import Data.Monoid

import System        ( getArgs )
import System.Random ( randomRIO )

data RegExp a = Epsilon
              | Symbol a
              | Star (RegExp a)
              | RegExp a :+: RegExp a
              | RegExp a :*: RegExp a

data Status a = Active a | Inactive a

isActive :: Status a -> Bool
isActive (Active _)   = True
isActive (Inactive _) = False

symbol :: Status a -> a
symbol (Active a)   = a
symbol (Inactive a) = a

accept :: Eq a => RegExp (Status a) -> [a] -> Bool
accept r []     = hasEpsilon r
accept r (c:cs) = isFinal . foldl next (activateFirst c r) $ cs

accepting :: Eq a => RegExp (Status a) -> [a] -> [RegExp (Status a)]
accepting r []     = [r]
accepting r (c:cs) = scanl next (activateFirst c r) $ cs

isFinal :: RegExp (Status a) -> Bool
isFinal = any isActive . last

last :: RegExp a -> [a]
last = first . flip

first :: RegExp a -> [a]
first = foldMapFirst (:[])

next :: Eq a => RegExp (Status a) -> a -> RegExp (Status a)

next Epsilon    _             = Epsilon

next (Symbol a) _             = Symbol (Inactive (symbol a))

next (Star r)   b | isFinal r = activateFirst b s
                  | otherwise = s
 where s = Star (next r b)

next (r :+: s)  b             = next r b :+: next s b

next (r :*: s)  b | isFinal r = next r b :*: activateFirst b t
                  | otherwise = next r b :*: t
 where t = next s b

activateFirst :: Eq a => a -> RegExp (Status a) -> RegExp (Status a)
activateFirst a = mapFirst activate
 where activate b | a == symbol b = Active a
       activate b                 = b

-- Boilerplate code

flip :: RegExp a -> RegExp a
flip Epsilon    = Epsilon
flip (Symbol a) = Symbol a
flip (Star r)   = Star (flip r)
flip (r :+: s)  = flip r :+: flip s
flip (r :*: s)  = flip s :*: flip r

hasEpsilon :: RegExp a -> Bool
hasEpsilon Epsilon    = True
hasEpsilon (Symbol _) = False
hasEpsilon (Star _)   = True
hasEpsilon (r :+: s)  = hasEpsilon r || hasEpsilon s
hasEpsilon (r :*: s)  = hasEpsilon r && hasEpsilon s

mapFirst :: (a -> a) -> RegExp a -> RegExp a
mapFirst _ Epsilon    = Epsilon
mapFirst f (Symbol a) = Symbol (f a)
mapFirst f (Star r)   = Star (mapFirst f r)
mapFirst f (r :+: s)  = mapFirst f r :+: mapFirst f s 
mapFirst f (r :*: s)  = mapFirst f r :*: t
 where t | hasEpsilon r = mapFirst f s
         | otherwise    = s

foldMapFirst :: Monoid m => (a -> m) -> RegExp a -> m
foldMapFirst _ Epsilon    = mempty
foldMapFirst f (Symbol a) = f a
foldMapFirst f (Star r)   = foldMapFirst f r
foldMapFirst f (r :+: s)  = foldMapFirst f r `mappend` foldMapFirst f s
foldMapFirst f (r :*: s)  = foldMapFirst f r `mappend` t
 where t | hasEpsilon r = foldMapFirst f s
         | otherwise    = mempty

instance Show (RegExp (Status Char)) where
  showsPrec _ Epsilon = id

  showsPrec _ (Symbol (Active c)) = showString $ red [c]

  showsPrec _ (Symbol (Inactive c)) = showString [c]

  showsPrec _ (Star r) = showsPrec 3 r . showString "*"

  showsPrec p (r :*: s) =
    showParen (p>2) (showsPrec 2 r . showsPrec 2 s)

  showsPrec p (r :+: s) =
    showParen (p>1) (showsPrec 1 r . showString "|" . showsPrec 1 s)

red :: String -> String
red s = "\ESC[91m" ++ s ++ "\ESC[0m"

-- Tests

evilRegExp :: Int -> RegExp (Status Char)
evilRegExp n = foldr (:*:) Epsilon $ replicate n (Epsilon:+:a) ++ replicate n a
 where a = Symbol (Inactive 'a')

regExp :: Int -> RegExp (Status Char)
regExp n = Star aOrB :*: Symbol (Inactive 'a')
       :*: foldr (:*:) Epsilon (replicate n aOrB)
       :*: Symbol (Inactive 'a') :*: Star aOrB
 where aOrB = Symbol (Inactive 'a') :+: Symbol (Inactive 'b')

main = do n <- (read.head) `fmap` getArgs
          s <- randomAB (n*n)
          putStrLn s
          mapM_ print $ accepting (regExp n) s
          print $ accept (regExp n) s

--           print $ accept (evilRegExp n) (replicate (2*n) 'a')

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