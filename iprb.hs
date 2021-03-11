import qualified Data.Map as Map
import Control.Monad (replicateM)

-- k homozygous dominant
-- m heterozygous
-- n homozygous recessive
--
-- Events D, H, R
--
-- pr(D&D) = [k / (k + m + n)] * [(k - 1) / (k + m + n - 1)]
-- pr(D&H) = [k / (k + m + n)] * [m / (k + m + n - 1)]
-- pr(D&R) = [k / (k + m + n)] * [n / (k + m + n - 1)]
--
-- pr(H&D) = [m / (k + m + n)] * [k / (k + m + n - 1)]
-- pr(H&H) = [m / (k + m + n)] * [(m - 1) / (k + m + n - 1)]
-- pr(H&R) = [m / (k + m + n)] * [n / (k + m + n - 1)]
--
-- pr(R&D) = [n / (k + m + n)] * [k / (k + m + n - 1)]
-- pr(R&H) = [n / (k + m + n)] * [m / (k + m + n - 1)]
-- pr(R&R) = [n / (k + m + n)] * [(n - 1) / (k + m + n - 1)]


data Organism = HomozygousDominant | Heterozygous | HomozygousRecessive
  deriving (Eq, Show, Ord, Bounded, Enum)

data Allele = Dominant | Recessive
  deriving (Eq, Show, Ord, Bounded, Enum)

type Population = Map.Map Organism Int


-- Everything!!
allOrganisms = [HomozygousDominant .. HomozygousRecessive]


-- Construct new population with `k`, `m`, and `n`.
newPopulation :: Int -> Int -> Int -> Population
newPopulation k m n = Map.fromList [(HomozygousDominant, k), (Heterozygous, m), (HomozygousRecessive, n)]


-- Get alleles for an organism
allelesOf :: Organism -> Map.Map Allele Int
allelesOf HomozygousDominant  = Map.fromList [(Dominant, 2), (Recessive, 0)]
allelesOf Heterozygous        = Map.fromList [(Dominant, 1), (Recessive, 1)]
allelesOf HomozygousRecessive = Map.fromList [(Dominant, 0), (Recessive, 2)]


-- Number of `organism`s inside `population`
count :: (Ord a) => a -> Map.Map a Int -> Float
count key population =
  countFor $ Map.lookup key population
  where countFor (Just val) = fromIntegral val
        countFor Nothing    = 0.0


-- Total number of organisms inside of `population`
total :: (Ord a) => Map.Map a Int -> Float
total population =
  fromIntegral $ sum $ Map.elems population


-- Remove one organism from the population
-- This kinda assumes the the given Population already has all possible keys..
remove :: (Ord a) => a -> Map.Map a Int -> Map.Map a Int
remove key population =
  Map.insertWith (-) key 1 $ population


-- Probability of picking a list of organisms in order
prPickList :: (Ord a) => [a] -> Map.Map a Int -> Float
prPickList [key] population = prPickOne key population
prPickList (key:others) population =
  (prPickOne key population) * (prPickList others $ remove key population)


-- Probability of picking one organism from the given population
prPickOne :: (Ord a) => a -> Map.Map a Int -> Float
prPickOne key population =
  (count key population) / (total population)


-- Probability of the given 2 organisms of hatching a dominant offspring
prDominantOffspring :: Organism -> Organism -> Float
prDominantOffspring organism1 organism2 =
  (pr Dominant organism1Alleles)  * (pr Dominant organism2Alleles)  +
  (pr Dominant organism1Alleles)  * (pr Recessive organism2Alleles) +
  (pr Recessive organism1Alleles) * (pr Dominant organism2Alleles)
  where organism1Alleles = allelesOf organism1
        organism2Alleles = allelesOf organism2
        pr allele x = (count allele x) / (total x)


-- Probability of picking these 2 organisms, AND them producing a dominant offspring
prPickAndDominant :: Organism -> Organism -> Population -> Float
prPickAndDominant organism1 organism2 population =
  (prPickList [organism1, organism2] population) * (prDominantOffspring organism1 organism2)


main = do
  -- print $ count HomozygousRecessive $ newPopulation 1 2 3
  -- print $ total $ newPopulation 1 2 3
  -- print $ remove HomozygousDominant $ newPopulation 1 2 3
  -- print $ prPickList [HomozygousDominant, HomozygousRecessive] (newPopulation 1 2 3)

  let pop = newPopulation 2 2 2
  let combos = replicateM 2 allOrganisms
  -- let combos = [ [HomozygousDominant, HomozygousDominant], [HomozygousDominant, Heterozygous], [HomozygousDominant, HomozygousRecessive], [Heterozygous, Heterozygous], [Heterozygous, HomozygousRecessive], [HomozygousRecessive, HomozygousRecessive] ]

  -- print $ prDominantOffspring HomozygousDominant HomozygousRecessive
  -- print $ prPickList [HomozygousDominant, HomozygousRecessive] pop
  print $ combos

  print $
    foldl (\acc comb -> acc + (prPickAndDominant (comb !! 0) (comb !! 1) pop)) 0.0 combos
