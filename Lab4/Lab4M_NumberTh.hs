{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use isDigit" #-}
import Numeric

{-# HLINT ignore "Redundant bracket" #-}


isPrime :: Int -> Bool
isPrime x = do
    let factors y = [x | x <- [1..y], mod y x == 0] --Calcule tous les facteurs d'un nombre
    length (factors x) == 2 --Un prime a uniquement 2 facteurs (1 et lui-meme)


pgcd :: Int -> Int -> Int
pgcd x y = if (y == 0 || x == 0 || x == y) then x else pgcd y (mod x y)


baseConverter :: Int -> Int -> [Int]
baseConverter _ 0 = []
baseConverter b x = do
    baseConverter b (div x b) ++ [mod x b]


toDecimal :: Int -> String -> Int
toDecimal b = foldl (\acc x -> acc * b + charToInt x) 0
  where
    charToInt c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'A' && c <= 'F' = 10 + fromEnum c - fromEnum 'A'


userPGCD :: IO()
userPGCD = do
    putStrLn "Entrez un premier nombre"
    x <- readLn
    putStrLn "Entrez un deuxieme nombre"
    y <- readLn
    putStrLn $ "Le PGCD de " ++ show x ++ " et " ++ show y ++ " est " ++ show (pgcd x y)

userPrime :: IO()
userPrime = do
    putStrLn "Entrez un nombre"
    x <- readLn
    putStrLn $ "Le nombre " ++ show x ++ " est premier -> " ++ show (isPrime x)

userBaseToDecimal :: IO()
userBaseToDecimal = do
    putStrLn "Entrez un nombre 'x' en base 'b'"
    x <- getLine
    putStrLn "Entrez une base 'b' quelconque"
    b <- readLn
    let xb10 = toDecimal b x
    --putStrLn $ "Le nombre " ++ show x ++ " en base " ++ show b ++ " est " ++ show xb10 ++ " en base 10"
    putStrLn "Entrez une base 'b2' a convertir x"
    b2 <- readLn
    print (baseConverter b xb10)
    putStrLn (concat $ hexNumbers $ baseConverter b2 xb10)


--decToHex :: Int -> String

hexNumbers :: [Int] -> [String]
hexNumbers a = map decToHex a
    where decToHex n = showHex n ""


userChoice :: Int -> IO()
userChoice 1 = userPGCD
userChoice 2 = userPrime
userChoice 3 = userBaseToDecimal
userChoice _ = putStrLn "Sortie de la console en cours..."


main :: IO ()
main = do
    putStrLn "Entrez un choix\n\ 
        \1) - pgcd moment\n\
        \2) - prime moment\n\
        \3) - base moment\n\
        \4) - Quitter la console\n"
    choice <- readLn
    userChoice choice
    r <- getLine
    putStrLn "Sortie de la console en cours..."