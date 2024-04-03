charToInt :: Char -> Int
charToInt = (+1) . (subtract (fromEnum 'A') . fromEnum) -- degeulasserie des plus bas fonds des enfers


isPrime :: Int -> Bool
isPrime x
    | x == 1 = False
    | otherwise = null [factor | factor <- [2..floor $ sqrt $ fromIntegral x], mod x factor == 0]


pgcd :: Int -> Int -> Int
pgcd x y = if y == 0 || x == 0 || x == y then x else pgcd y (mod x y)


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
            putStrLn "\n--- --- --- --- --- ---\nEntrez des valeurs valides !!!"
            putStrLn $ "p prime? : " ++ show (isPrime p)
            putStrLn $ "q prime? : " ++ show (isPrime q)
            putStrLn $ "p*q>=1000? : " ++ show (p*q >= 1000)
            putStrLn $ "e,(p-1 * q-1)  coprime? : " ++ show (pgcd e ((p-1)*(q-1)) == 1) ++ "\n--- --- --- --- --- ---\n"
            userInit


userRSA :: IO()
userRSA = do
    
    (p,q,e) <- userInit
    putStrLn "Entrez votre message (A-Z)"
    m <- getLine
    let mInt = concatMap (paddington 2 . charToInt) m --Cree un string des chiffres (BONJOUR -> "02151410152118")
    let strList = map reverse $ reverse $ chunkOf (reverse mInt) 3 --separe le string en liste de 3 char ("02151410152118" -> ["02","151","410","152","118"])
    let intList = [read x :: Int | x <- strList] -- convertit les strings en Int (["02","151","410","152","118"] -> [2,151,410,152,118])
    let test = map (\x -> encode x e (p*q)) intList -- applique la fonction 'encode' pour la liste (2,151,410,152,118] -> [128,800,3761,660,204])
    --print mInt --print strList --print intList
    putStrLn $ "\nMessage encode :\n" ++ show test

strToLInt :: [[Char]] -> [Int]
strToLInt xs = map (\c -> read [c]) (concat xs)  --'read' prend un str et converti vers un type, puis le fait pour tout les elements de la liste


paddington :: Int -> Int -> [Char]
paddington n x = take (n - length sx) (cycle "0") ++ sx
        where sx = show x --sx est la representation en string de x

chunkOf :: [Char] -> Int -> [[Char]]
chunkOf [] _ = []
chunkOf l n = take n l : chunkOf (drop n l) n --prend les n premiers elements de la liste et les met dans une liste, puis refait la meme chose avec le reste de la liste

encode :: Int -> Int -> Int -> Int
encode m e n = mod (m^e) n


userChoice :: Int -> IO()
userChoice 1 = do
    userRSA
userChoice _ = putStrLn "Sortie de la console en cours..."


main :: IO ()
main = do
    putStrLn "Entrez un choix\n\ 
        \1) - Encodage RSA\n\
        \4) - Quitter la console\n"
    choice <- readLn
    userChoice choice
    --r <- getLine
    --putStrLn "Sortie de la console en cours..."
