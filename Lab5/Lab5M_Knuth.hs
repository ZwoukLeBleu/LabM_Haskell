{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use isDigit" #-}
{-# HLINT ignore "Redundant bracket" #-}


isPrime :: Int -> Bool
isPrime x
    | x == 1 = False
    | otherwise = null [factor | factor <- [2..floor $ sqrt $ fromIntegral x], mod x factor == 0]   
    --Ici, 'null' n'est pas un type comme dans les autres langue,
    --mais plutot une fonction qui regarde si la liste est vide et returns True si vide. 
    --lalgorithme est different du lab4 car il est plus efficace (du moins je crois)
    --et car il ne donne pas de faux-positifs comme le precedent
pgcd :: Int -> Int -> Int
pgcd x y = if (y == 0 || x == 0 || x == y) then x else pgcd y (mod x y) --lab4


primeList :: Int -> [Int] 
primeList m = filter isPrimeFactor [2..floor $ sqrt $ fromIntegral m] 
    where isPrimeFactor x = mod m x == 0 && isPrime x
    --Pour tous le chiffres entier entre 2 et sqrt(m), on filtre tout ceux qui ne sont pas des facteurs premiers


firstCriteria :: Int -> Int -> Bool
firstCriteria c m = pgcd c m == 1 
    

secondCriteria :: [Int] -> Int -> Bool
secondCriteria [] a = True
secondCriteria (x:xs) a = (mod (a-1) x == 0) && secondCriteria xs a


thirdCriteria :: Int -> Int -> Bool
thirdCriteria m a = do
    if (mod m 4 == 0) 
        then (mod (a-1) 4 == 0)
        else True


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
        else putStrLn "Knuth ne peut pas generer des nombres pseudo-aleatoires avec ces valeurs"
            

printKnuth :: (Int, Int, Int, Int) -> Int -> IO()
printKnuth (a, m, c, x) i = do
    if i<20 --voici une for loop dans Haskell
        then do
            let xc = mod (a*x + c) m
            putStr $ show xc ++ ", "
            printKnuth (a, m, c, xc) (i+1)
        else putStrLn "... etc"


userChoice :: Int -> IO()
userChoice 1 = userKnuth
userChoice _ = putStrLn "Sortie de la console en cours..."


main :: IO ()
main = do
    putStrLn "Entrez un choix\n\ 
        \1) - Knuth\n\
        \2) - Quitter la console\n"
    choice <- readLn
    userChoice choice
    --r <- getLine
    --putStrLn "Sortie de la console en cours..."
