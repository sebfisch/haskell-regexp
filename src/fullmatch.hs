import System.Environment ( getArgs )
import Text.RegExp        ( fromString, acceptFull )

main :: IO ()
main = do re:_ <- getArgs
          str <- getContents
          print $ acceptFull (fromString re) str

{-

Matching 100 MB of a's against .* takes 8 seconds (that is 16 MB/s)
and runs in constant space (1 MB).

bash# python -c 'import sys; sys.stdout.write(100*1024*1024*"a")' | ./fullmatch ".*" +RTS -s
./fullmatch .* +RTS -s 
True
   7,316,475,040 bytes allocated in the heap
      23,734,320 bytes copied during GC
          10,048 bytes maximum residency (1 sample(s))
          11,324 bytes maximum slop
               1 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0: 13918 collections,     0 parallel,  0.17s,  0.21s elapsed
  Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    8.16s  (  9.26s elapsed)
  GC    time    0.17s  (  0.21s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time    8.33s  (  9.47s elapsed)

  %GC time       2.0%  (2.2% elapsed)

  Alloc rate    896,396,702 bytes per MUT second

  Productivity  98.0% of total user, 86.2% of total elapsed

Matching 5000 a's against (a?){5000}a{5000} takes 13 seconds and 5
MB. Almost 10 seconds are spent during garbage collection.

bash# python -c 'import sys; sys.stdout.write(5000*"a")' | ./fullmatch "(a?){5000}a{5000}" +RTS -s
./fullmatch (a?){5000}a{5000} +RTS -s 
True
   3,763,594,176 bytes allocated in the heap
   3,054,690,828 bytes copied during GC
       1,523,216 bytes maximum residency (502 sample(s))
          49,040 bytes maximum slop
               5 MB total memory in use (0 MB lost due to fragmentation)

  Generation 0:  6675 collections,     0 parallel,  8.29s,  8.38s elapsed
  Generation 1:   502 collections,     0 parallel,  1.21s,  1.24s elapsed

  INIT  time    0.00s  (  0.00s elapsed)
  MUT   time    3.53s  (  3.60s elapsed)
  GC    time    9.50s  (  9.61s elapsed)
  EXIT  time    0.00s  (  0.00s elapsed)
  Total time   13.03s  ( 13.21s elapsed)

  %GC time      72.9%  (72.7% elapsed)

  Alloc rate    1,065,771,832 bytes per MUT second

  Productivity  27.1% of total user, 26.7% of total elapsed

-}