{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use isDigit" #-}
import Numeric
import Data.Int


{-# HLINT ignore "Redundant bracket" #-}


isPrime :: Int -> Bool
isPrime 1 = False
isPrime x = do    
    let factors y = [x | x <- [1..y], mod y x == 0] --Calcule tous les facteurs d'un nombre
    length (factors x) == 2 --Un prime a uniquement 2 facteurs (1 et lui-meme)


pgcd :: Int -> Int -> Int
pgcd x y = if (y == 0 || x == 0 || x == y) then x else pgcd y (mod x y)


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

userInit :: IO((Int, Int, Int, Int))
userInit = do
    putStrLn "Entrez 'a'"
    a <- readLn
    putStrLn "Entrez 'm'"
    m <- readLn
    putStrLn "Entrez 'c'"
    c <- readLn
    putStrLn "Entrez 'x'"
    x <- readLn
    if True--2<=a && a<m && 0<=c && c<m && 0<=x && x<m 
        then return (a, m, c, x)
        else do
            putStrLn "\nEntrez des valeurs valides !!!"
            userInit

userKnuth ::  IO()
userKnuth  = do
    (a, m, c, x) <- userInit
    putStrLn $ "Premier critere: " ++ show (firstCriteria c m)
    putStrLn $ "Deuxieme critere: " ++ show (secondCriteria (primeList m) a)
    putStrLn $ "Troisieme critere: " ++ show (thirdCriteria m a) ++ "\n"
    print $ primeList m
    if firstCriteria c m && secondCriteria (primeList m) a && thirdCriteria m a
        then printKnuth (a, m, c, x) 0
        else putStrLn "-1"
            

printKnuth :: (Int, Int, Int, Int) -> Int -> IO()
printKnuth (a, m, c, x) i = do
    if i<20
        then do
            let xc = mod (a*x + c) m
            putStr $ show xc ++ ", "
            printKnuth (a, m, c, xc) (i+1)
        else putStrLn "Fin de la boucle"


primeList :: Int -> [Int]
primeList m = [y | y <- [1..floor(sqrt (fromIntegral m))], isPrime y]

firstCriteria :: Int -> Int -> Bool
firstCriteria c m = pgcd c m == 1
    

secondCriteria :: [Int] -> Int -> Bool
secondCriteria [] a = True
secondCriteria (x:xs) a = do
    if mod (a-1) x == 0
        then secondCriteria xs a
        else False

thirdCriteria :: Int -> Int -> Bool
thirdCriteria m a = do
    if mod m 4 == 0
        then mod (a-1) 4 == 0
        else True
    



userChoice :: Int -> IO()
userChoice 1 = do
    --print $ primeList 80
    --print $ secondCriteria (primeList 12) 13
    userKnuth
userChoice _ = putStrLn "Sortie de la console en cours..."


main :: IO ()
main = do
    putStrLn "Entrez un choix\n\ 
        \1) - priemlist moment\n\
        \4) - Quitter la console\n"
    choice <- readLn
    userChoice choice
    r <- getLine
    putStrLn "Sortie de la console en cours..."