import Control.Monad
import Control.Monad.MC

f a i = map b [1 .. ai] 
  where
    ai = a !! (i-1)
    b j
      | j == 1              = ai + 1
      | 1 < j && j <= i     = min (i + 1) (a !! (j - 1))
      | i < j && j <= ai    = a !! (j - 2) + 1
      | otherwise           = undefined

g a n
  | n == 1    = sum a
  | n > 1     = sum $ map (\i -> g (f a i) (n - 1)) [1 .. length a]
  | otherwise = undefined

enumerate = g [1]

data Label = Label {
--    permutation :: [Int],
    activeSites ::  [Int],
    choosen :: Int 
    } deriving (Show)

expand :: Label -> [Label]
expand (Label a i) = [ Label (f a i) j | j <- [1 .. a !! (i - 1)] ]

seed = Label [2] 1

randomExpand:: Label -> MC Label
randomExpand label = sample $ expand label

randomNodeAtLevel n = replicateM n randomExpand seed

grow :: [Label] -> MC [Label]
grow (l:ls) = do
    x <- randomExpand l
    let y = x:l:ls
    return y

growRepeatedly n = foldl1 (>=>) $ replicate n grow

numberActiveSites :: Label -> Int
numberActiveSites = length . activeSites

numberActiveSitesExcursion n = map numberActiveSites `fmap` growRepeatedly n [seed]

getInt :: IO Int
getInt = do
 s <- getLine
 return (read s)

getSeed :: IO Seed
getSeed = do
    s <- getLine
    return (read s)

myMC n m = replicateMC m $ numberActiveSitesExcursion n

main = do
 n <- getInt
 m <- getInt
 seed_line <- getLine
 let seed = read seed_line :: Seed
 mapM_ print $ evalMC (myMC n m) (mt19937 seed)
