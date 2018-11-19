module Main where

import System.Random
import Data.Char

main :: IO ()
main = do
    gen1 <- newStdGen
    gen2 <- newStdGen
    gen3 <- newStdGen
    let r@( p, q, phi, n, e, d) = prepareRSA gen1 gen2 gen3
    --putStr $ show r
    --putStrLn "   :: ( p, q, n, e, d)"
    putStrLn $ "p   = " ++ show p
    putStrLn $ "q   = " ++ show q
    putStrLn $ "phi = " ++ show phi
    putStrLn $ "n   = " ++ show n
    putStrLn $ "e   = " ++ show e
    putStrLn $ "d   = " ++ show d

    putStrLn message
    let text = fmap (fromIntegral . ord) message
    putStrLn $ show text

    let enc = rsa text e n
    putStrLn $ show enc

    let dec = rsa enc d n
    putStrLn $ show dec
    putStrLn $ fmap (chr . fromIntegral) dec

sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]

primes = sieve [2..]

primes4D = takeWhile (<10000) $ dropWhile (<1000) primes

getPQ gen = let [a,b] = take 2 $ randomRs (0,primesLen) gen 
                primesLen = length primes4D
            in (primes4D !! a, primes4D !! b)
    
prepareRSA :: (RandomGen g) => g -> g -> g -> (Integer , Integer, Integer, Integer, Integer, Integer)
prepareRSA gen1 gen2 gen3 = ( p, q, phi, n, e, d)
    where (p,q) = getPQ gen1
          n = p*q
          phi = (p-1)*(q-1)
          e = head $ [x | x <- [2..] , gcd x phi == 1]
          d = head $ [x | x <- [2..], mod (e*x-1) phi == 0]

randomPrimes :: (RandomGen g) => g -> [Integer]
randomPrimes gen = let r = randomRs (0,100) gen
                    in fmap (\x -> primes !! x) r

cryptRSA :: Integer -> Integer -> Integer -> Integer
cryptRSA c key n = fastPow c key n

rsa :: [Integer] -> Integer -> Integer -> [Integer]
rsa t key n = fmap (\c -> cryptRSA c key n) t

message = "But look! here come more crowds, pacing straight for the water, and seemingly bound for a dive. Strange!"

fastPow :: Integer -> Integer -> Integer -> Integer
fastPow base 1 m = mod base m
fastPow base pow m | even pow = mod ((fastPow base (div pow 2) m) ^ 2) m
                   | odd  pow = mod ((fastPow base (div (pow-1) 2) m) ^ 2 * base) m
