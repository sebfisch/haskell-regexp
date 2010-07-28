import System.Environment ( getArgs )
import Text.RegExp        ( fromString, acceptPartial )

main :: IO ()
main = do re:_ <- getArgs
          str <- getContents
          print $ acceptPartial (fromString re) str

{-

Failing to find a substring of the form `a.{20}a` in about 2 million
randomly generated a's and b's takes 3.6 seconds and 1 MB:

bash# ./genrandom 20 100000 | ./partialmatch a.{20}a +RTS -s
./partialmatch a.{20}a +RTS -s 
False
   3,436,823,084 bytes allocated in the heap
      16,073,588 bytes copied during GC
          15,188 bytes maximum residency (1 sample(s))
          15,392 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:  6554 collections,     0 parallel,  0.10s,  0.12s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    3.57s  (  4.04s elapsed)
  GC    time    0.10s  (  0.12s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    3.67s  (  4.16s elapsed)

  %GC time       2.7%  (2.8% elapsed)

  Alloc rate    961,935,693 bytes per MUT second

  Productivity  97.3% of total user, 85.8% of total elapsed

-}