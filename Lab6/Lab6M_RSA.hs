
charToInt :: Char -> Int
charToInt = (+1) . (subtract (fromEnum 'A') . fromEnum)


isPrime :: Int -> Bool
isPrime x
    | x == 1 = False
    | otherwise = null [factor | factor <- [2..floor $ sqrt $ fromIntegral x], mod x factor == 0]


pgcd :: Int -> Int -> Int
pgcd x y = if (y == 0 || x == 0 || x == y) then x else pgcd y (mod x y)


userInit :: IO(Int, Int, Int)
userInit = do
    putStrLn "Entrez 'p'"
    p <- readLn
    putStrLn "Entrez 'q'"
    q <- readLn
    putStrLn "Entrez 'e'"
    e <- readLn
    if isPrime p && isPrime q && p*q >= 1000 && pgcd e ((p-1)*(q-1)) == 1
        then return (p, q, e)
        else do
            print $ isPrime p
            print $ isPrime q
            print (p*q >= 1000)
            print (pgcd e ((p-1)*(q-1)))
            putStrLn "\nEntrez des valeurs valides !!!"
            userInit


userRSA :: IO()
userRSA = do
    (p,q,e) <- userInit
    m <- getLine
    let mInt = concatMap (paddington 2 . charToInt) m --concat $ map
    print mInt
    let strList = map reverse $ reverse $ chunkOf (reverse mInt) 3
    let intList = [read x :: Int | x <- strList]
    let test = map (\x -> encode x e (p*q)) intList



    print strList
    print intList
    print test

strToLInt :: [[Char]] -> [Int]
strToLInt xs = map (\c -> read [c]) (concat xs)


paddington :: Int -> Int -> [Char]
paddington n x = take (n - length sx) (cycle "0") ++ sx
        where sx = show x

chunkOf :: [Char] -> Int -> [[Char]]
chunkOf [] _ = []
chunkOf l n = take n l : chunkOf (drop n l) n

encode :: Int -> Int -> Int -> Int
encode m e n = mod (m^e) n


userChoice :: Int -> IO()
userChoice 1 = do
    userRSA
userChoice _ = putStrLn "Sortie de la console en cours..."


main :: IO ()
main = do
    putStrLn "Entrez un choix\n\ 
        \1) - priemlist moment\n\
        \4) - Quitter la console\n"
    choice <- readLn
    userChoice choice
    --r <- getLine
    --putStrLn "Sortie de la console en cours..."