{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use replicateM" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use head" #-}
import Data.List

customMatrix ::  Int -> Int -> IO [[Double]]
customMatrix m n = do
    putStrLn ("Entrez " ++ show n ++ " chiffres separer par des espaces. Appuyer sur [ENTER] pour changer de ligne.")
    let readRow = do
            line <- getLine
            return (map read (words line))
    sequence (replicate m readRow)


transposeMatrix :: [[Double]] -> [[Double]]
transposeMatrix ([]:_) = [] --Definition necessaire pour que la recursion du programme s'arrete quand il ni a plus de colonnes a transposer
transposeMatrix matrix = map head matrix : transposeMatrix (map tail matrix)


produitScalaire :: [Double] -> [Double] -> Double
produitScalaire x y = sum (zipWith (*) x y)


smallDeteminant :: [[Double]] -> Double
smallDeteminant matrix = (w*x) - (y*z)
    where 
        [w, x] = matrix!!0
        [y, z] = matrix!!1 


multiplyMatrix :: [[Double]] -> [[Double]] -> [[Double]] 
multiplyMatrix a b = [[ produitScalaire ar bc | bc <- transposeMatrix b ] | ar <- a ] 


--retire l'element i d'une liste
removeCol :: [Double] -> Int -> [Double]
removeCol (_:as) 0 = as --Si i = 0, garde tout ce qui n'est pas la premiere valeur et return
removeCol (a:as) i = a : removeCol as (i - 1)   --autrement, garde x, mais refait removeCol (i-1) avec le restant de la liste
                                                --Somme toute, ca passe a travers la liste 1 element a la fois, de gauche a droite


--retire une colonne au complet d'une matrice. Utiliser pour le determinant d'une matrice plus grande que 1x1 ou 2x2
removeMultiCol :: [[Double]] -> Int -> [[Double]]
removeMultiCol [] _ = []
removeMultiCol (a:as) i = removeCol a i : removeMultiCol as i


getDetSumElement :: [[Double]] -> Int -> Double
getDetSumElement matrix i = ((-1) ^ i) * element * detSum restOfTheMatrix 0
    where
        element = (matrix !! 0 !! i)
        restOfTheMatrix = removeMultiCol (tail matrix) i


detSum :: [[Double]] -> Int -> Double
detSum matrix i | len == 1 = (matrix !! 0 !! 0)
                | len == i = 0
                | otherwise = currentSumElement + detSum matrix (i + 1)
                    where
                       currentSumElement = getDetSumElement matrix i
                       len = length matrix


-- Similaire a removeCol, mais a la place, remplace l'element i de a par b 
replaceCol :: [Double] -> [Double] -> Int -> [Double]
replaceCol [] _ _ = []
replaceCol a b x =
    let (aa, ab) = splitAt x a  --Coupte la liste en deux, a la position x
    in aa ++ (head b : drop 1 ab)   --La liste aa est mise en avant, puis, on y 'append' la premiere valeur de b (head b)
                                    --et on retire toutes les valeurs avant ab!!1 de ab avant de l'append a la liste finale


--exactement le meme fonctionement que removeMultiCol, mais a la place, remplace la colonne i de a par b
replaceMultiCol :: [[Double]] -> [[Double]] -> Int -> [[Double]]
replaceMultiCol [] [] _ = []
replaceMultiCol (a:as) (b:bs) x = replaceCol a b x : replaceMultiCol as bs x


--C'est la fonction qui cree la matrice solution avec la regle de Cramer
cramerList1 :: [[Double]] -> [[Double]] -> [[Double]]
cramerList1 a b = [[detNewMatrix i / detA] | i <- [0..(length a - 1)]] --Applique deltaA/deltaX pour toutes les colonnes de la matrice
    where
        detA = detSum a 0
        detNewMatrix i = let newMatrix = replaceMultiCol a b i in detSum newMatrix 0
        --Pour chaque colonne de la matrice, on remplace la colonne i par la colonne b, puis on trouve le determinant de cette nouvelle matrice.
        --Le double = sur une seule variable est vraiment pas 'beau', mais je trouve ca tellement drole que je le laisse la


-- check si detA = 0
isThisSELCramerable :: [[Double]] -> Bool
isThisSELCramerable matrix = detSum matrix 0 /= 0


rangeChecker :: Int -> Int -> IO Int
rangeChecker lowerB upperB = do
    nbr <- readLn
    if nbr >= lowerB && nbr <= upperB
        then return nbr
        else do
            putStrLn "Veuillez reessayer. Ce nombre n'est pas dans la plage de valeurs acceptable."
            rangeChecker lowerB upperB


userPrefMatrices :: IO (Int, Int)
userPrefMatrices = do
    putStrLn "\n\nEntrez la longeur de colonne de matrice #1 (1 - 10)"
    a <- rangeChecker 1 3
    putStrLn "Entrez la longeur de ligne de matrice #1 (1 - 10)"
    b <- rangeChecker 1 3
    return (a, b)


userSELResolution :: IO()
userSELResolution = do
    (m, n) <- userPrefMatrices
    aMatrix <- customMatrix m n
    mapM_ print aMatrix
    bMatrix <- customMatrix m 1
    mapM_ print bMatrix
    if isThisSELCramerable aMatrix
        then do
            putStrLn "\nCe systeme d'equations lineaires est faisable car il n'a qu'une seule solution possible"
            let result = cramerList1 aMatrix bMatrix
            putStrLn "\nLa matrice solution est:"
            mapM_ print result
            putStrLn $ "La solution est donc: x = " ++ show (result!!0!!0) ++ ", y = " ++ show (result!!1!!0) ++ "et z = " ++ show (result!!2!!0)
        else do 
            putStrLn "\nCe systeme d'equations lineaires n'est pas faisable selon la regle de Cramer, car..."
            if (bMatrix!!0!!0 == bMatrix!!1!!0 && bMatrix!!0!!0 == bMatrix!!2!!0)
                then putStrLn "... il n'y a pas de solution unique"
                else putStrLn "... il n'y a pas de solution possible"


userChoice :: Int -> IO()
userChoice 1 = userSELResolution
userChoice _ = putStrLn "Sortie de la console en cours..."


--La fonction qui fait tout
main :: IO ()
main = do
    putStrLn "Entrez un choix\n\ 
        \1) - Trouver la solution d'un SEL a trois equations et trois inconnus\n\
        \2) - Quitter la console\n"
    choice <- readLn
    userChoice choice
    r <- readLn
    putStrLn r --Pour que la console ne se ferme pas immediatement