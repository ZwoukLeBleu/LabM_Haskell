{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use replicateM" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.List -- J'importe les lists[] car cest essentiel et sans les listes, c'est pratiquement impossible a faire en Haskell


{- EXPLICATION!!
'replicate  :   Repete x fois l'action. Dans notre cas, ca repete {ORDRE_MATRICE} fois la fonction qui demande le user input.
                A chaque repetition, les valeurs sont encapsuler dans une liste.
'sequence'  :   Place toute les valeurs qui appartient a sa parenthese dans une belle liste toute fraiche. On fini donc avec [[Tnt]]! -}
customMatrix :: Int -> Int -> IO [[Int]]
customMatrix m n = do
    putStrLn ("Entrez " ++ show n ++ " chiffres separer par des espaces. Appuyer sur [ENTER] pour changer de ligne.")
    let readRow = do
            line <- getLine
            return (map read (words line))
    sequence (replicate m readRow)


{- EXPLICATIONS!!
'n:otherNs' :   Le 'a' est la premiere ligne de la premiere matrice. 'as' est toute les autres lignes de cette meme matrice. 
'zipWith'   :   Ici, zipWith est appliquer sur les listes 'a' et 'b' avec l'operateur d'addition.
                Cela fait l'operation 'a+b' et recrache (dans une liste) les resultat. -}
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
--addMatrices [] [] = []
addMatrices (a:as) (b:bs) = (zipWith (+) a b) : addMatrices as bs


{- EXPLICATION!!
'map'       :   'map' veux dire que je veux documenter tout les elements (listes) dans ma liste de liste de int. 
'head '     :   head est une fonction de base qui va prendre uniquement le premier element d'une liste.
                Puisque on a plusieurs listes pour chaque matrices, on prend les elements qui appartient a (1,n) et on les places dans une nouvelle liste 
                Par exemple, avec [[1, 2], [3, 4]], on ressort [1, 3]. On reitere la fonction, et maintenant on ressort [2, 4].
                la combinaison des deux listes donne [[1,3], [2,4]], soit la matrice transposer!!!!! -}
transposeMatrix :: [[Int]] -> [[Int]]
transposeMatrix ([]:_) = [] --Definition necessaire pour que la recursion du programme s'arrete quand il ni a plus de colonnes a transposer
transposeMatrix matrix = map head matrix : transposeMatrix (map tail matrix)


--je sais pas cest quoi le mot en anglais donc voici la seule fonction en francais :)
produitScalaire :: [Int] -> [Int] -> Int
produitScalaire x y = sum (zipWith (*) x y)


smallDeteminant :: [[Int]] -> Int
smallDeteminant matrix = (w*x) - (y*z)
    where 
        [w, x] = matrix!!0
        [y, z] = matrix!!1 

multiplyMatrix :: [[Int]] -> [[Int]] -> [[Int]] 
multiplyMatrix a b = [[ produitScalaire ar bc | bc <- transposeMatrix b ] | ar <- a ] 


removeCol :: [Int] -> Int -> [Int]
removeCol (_:as) 0 = as --Si i = 0, garde tout ce qui n'est pas la premiere valeur et return
removeCol (a:as) i = a : removeCol as (i - 1)   --autrement, garde x, mais refait removeCol (i-1) avec le restant de la liste
                                                --Somme toute, ca passe a travers la liste 1 element a la fois, de gauche a droite

-- Remove col i from all rows of the matrix
removeMultiCol :: [[Int]] -> Int -> [[Int]]
removeMultiCol [] _ = []
removeMultiCol (a:as) i = removeCol a i : removeMultiCol as i


getDetSumElement :: [[Int]] -> Int -> Int
getDetSumElement matrix i = ((-1) ^ i) * element * detSum restOfTheMatrix 0
    where
        element = fromIntegral $ matrix !! 0 !! i
        restOfTheMatrix = removeMultiCol (tail matrix) i


detSum :: [[Int]] -> Int -> Int
detSum matrix i | len == 1 = fromIntegral $ matrix !! 0 !! 0
                | len == i = 0
                | otherwise = currentSumElement + detSum matrix (i + 1)
                    where
                       currentSumElement = getDetSumElement matrix i
                       len = length matrix


userDeterminantMatrix :: IO()
userDeterminantMatrix = do
    putStrLn "Entrez l'ordre de la matrice a transposer: "
    order <- readLn
    cMatrix <- customMatrix order order
    let det = detSum cMatrix 0
    mapM_ print cMatrix
    print det


rangeChecker :: Int -> Int -> IO Int
rangeChecker lowerB upperB = do
    nbr <- readLn
    if nbr >= lowerB && nbr <= upperB
        then return nbr
        else do
            putStrLn "Veuillez reessayer. Ce nombre n'est pas dans la plage de valeurs acceptable."
            rangeChecker lowerB upperB


userPrefMatrices :: IO (Int, Int, Int, Int)
userPrefMatrices = do
    putStrLn "Entrez la longeur de colonne de matrice #1 (1 - 10)"
    a <- rangeChecker 1 10
    putStrLn "Entrez la longeur de ligne de matrice #1 (1 - 10)"
    b <- rangeChecker 1 10
    putStrLn "Entrez la longeur de colonne de matrice #2 (1 - 10)"
    c <- rangeChecker 1 10
    putStrLn "Entrez la longeur de ligne de matrice #2 (1 - 10)"
    d <- rangeChecker 1 10
    return (a, b, c, d)


userAddMatrix :: IO()
userAddMatrix = do
    (m1, n1, m2, n2) <- userPrefMatrices
    putStrLn ""
    uMatrix1 <- customMatrix m1 n1
    mapM_ print uMatrix1
    putStrLn ""
    uMatrix2 <- customMatrix m2 n2
    mapM_ print uMatrix2
    putStrLn ""
    let results = addMatrices uMatrix1 uMatrix2
    mapM_ print results


userTransposeMatrix :: IO()
userTransposeMatrix = do
    putStrLn "Entrez l'ordre de la matrice a transposer: "
    order <- readLn
    tMatrix <- transposeMatrix <$> customMatrix order order --magie noir
    mapM_ print tMatrix


userMultiplyMatrices:: IO()
userMultiplyMatrices = do
    (m1, n1, m2, n2) <- userPrefMatrices
    --print (m1, n1, m2, n2)
    if n1 /= m2
        then do
            putStrLn "\nVeuillez reessayer. Les matrices ne sont pas compatible.\n"
            userMultiplyMatrices
        else do
            putStrLn ""
            uMatrix1 <- customMatrix m1 n1
            mapM_ print uMatrix1
            putStrLn ""
            uMatrix2 <- customMatrix m2 n2
            mapM_ print uMatrix2
            putStrLn ""
            let results = multiplyMatrix uMatrix1 uMatrix2
            mapM_ print results



userChoice :: Int -> IO()
userChoice 1 = userAddMatrix
userChoice 2 = userTransposeMatrix
userChoice 3 = userMultiplyMatrices
userChoice 4 = userDeterminantMatrix
userChoice _ = putStrLn "Sortie de la console en cours..."


--La fonction qui fait tout
main :: IO ()
main = do
    putStrLn "Entrez un choix\n\ 
        \1) - Addition de deux matrices valides\n\
        \2) - Transposition d'une matrice valide\n\
        \3) - Multiplication de deux matrices valides\n\
        \4) - Calculer le determinant d'une matrice valide\n\
        \5) - Quitter la console\n"
    choice <- readLn
    userChoice choice


