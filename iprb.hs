import qualified Data.Map as Map

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


data Organism = HomozygousDominent | Heterozygous | HomozygousRecessive
  deriving (Eq, Show, Ord, Bounded, Enum)

data Allele = Dominent | Recessive
  deriving (Eq, Show, Ord, Bounded, Enum)

type Population = Map.Map Organism Int


-- Construct new population with `k`, `m`, and `n`.
newPopulation :: Int -> Int -> Int -> Population
newPopulation k m n = Map.fromList [(HomozygousDominent, k), (Heterozygous, m), (HomozygousRecessive, n)]


-- Get alleles for an organism
allelesOf :: Organism -> Map.Map Allele Int
allelesOf HomozygousDominent  = Map.fromList [(Dominent, 2), (Recessive, 0)]
allelesOf Heterozygous        = Map.fromList [(Dominent, 1), (Recessive, 1)]
allelesOf HomozygousRecessive = Map.fromList [(Dominent, 0), (Recessive, 2)]


-- Number of `organism`s inside `population`
popCount :: (Ord a) => a -> Map.Map a Int -> Float
popCount key population =
  countFor $ Map.lookup key population
  where countFor (Just val) = fromIntegral val
        countFor Nothing    = 0.0


-- Total number of organisms inside of `population`
popTotal :: (Ord a) => Map.Map a Int -> Float
popTotal population =
  fromIntegral $ sum $ Map.elems population


-- Remove one organism from the population
-- This kinda assumes the the given Population already has all possible keys..
popRemove :: (Ord a) => a -> Map.Map a Int -> Map.Map a Int
popRemove key population =
  Map.insertWith (-) key 1 $ population


-- Probability of picking a list of organisms in order
prPickList :: (Ord a) => [a] -> Map.Map a Int -> Float
prPickList [key] population = prPickOne key population
prPickList (key:others) population =
  (prPickOne key population) * (prPickList others $ popRemove key population)


-- Probability of picking one organism from the given population
prPickOne :: (Ord a) => a -> Map.Map a Int -> Float
prPickOne key population =
  (popCount key population) / (popTotal population)


-- TODO
-- TODO
-- TODO
-- TODO
-- TODO is this useable? the "population" changes a bit differently once we're talking about alleles...
-- TODO before, it's one pop but now it's two. pr(pick one form a) and pr(pick one from b) now...
-- TODO
-- TODO
-- TODO
prDominent :: Organism -> Float
prDominent organism =
  (popCount Dominent a) / (popTotal a)
  where a = allelesOf organism


main = do
  -- print $ popCount HomozygousRecessive $ newPopulation 1 2 3
  -- print $ popTotal $ newPopulation 1 2 3
  -- print $ popRemove HomozygousDominent $ newPopulation 1 2 3
  -- print $ prPickList [HomozygousDominent, HomozygousRecessive] (newPopulation 1 2 3)

  let pop = newPopulation 2 2 2

  print $
    ((prPickList [HomozygousDominent] pop) * 1.0) +
    ((prPickList [Heterozygous, Heterozygous] pop) * 1.0/2.0) +
    ((prPickList [Heterozygous, HomozygousRecessive] pop) * 1.0/4.0)
