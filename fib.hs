import Text.Printf
import Control.Exception
import System.CPUTime
(//) = div
(%) = mod
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

times = do
    putStrLn "fib1(26)"
    time $ fib1    26 `seq` return ()
    putStrLn "fib2(150000)"
    time $ fib3   150000 `seq` return ()
    putStrLn "fib3(150000)"
    time $ fib4 150000 `seq` return ()
    putStrLn "Done."

type Number = Integer

fib1 :: Number -> Number
fib1 0 = 0
fib1 1 = 1
fib1 n = (fib1 (n-1)) + (fib1 (n-2))


phi = (1 + (sqrt 5)) / 2
ihp = (1 - (sqrt 5)) / 2

fib2 :: Double -> Double 
fib2 n = ((phi ** n) - (ihp ** n)) / (sqrt 5)

fib3 :: Number -> Number
fib3 n = climbTo n (0, 1)

climbTo :: Number -> (Number, Number) -> Number
climbTo 0 (a,b) = a
climbTo n (a,b) = climbTo (n - 1) (b, a + b)

fib4 :: Number -> Number 
fib4 0 = 1
fib4 1 = 1
fib4 n = 
  let x     = (n-2)//2 in 
  let (a,b) = (fib4 x, fib4 (x+1)) in
  if even n 
    then a*a + b*b 
    else a*b + b*(a+b)