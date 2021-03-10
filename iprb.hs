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

type Population = Map.Map Organism Int


-- Construct new population with `k`, `m`, and `n`.
newPopulation :: Int -> Int -> Int -> Population
newPopulation k m n = Map.fromList [(HomozygousDominent, k), (Heterozygous, m), (HomozygousRecessive, n)]


-- Number of `organism`s inside `population`
popCount :: Organism -> Population -> Float
popCount organism population =
  countFor $ Map.lookup organism population
  where countFor (Just val) = fromIntegral val
        countFor Nothing    = 0.0


-- Total number of organisms inside of `population`
popTotal :: Population -> Float
popTotal population =
  fromIntegral $ sum $ Map.elems population


-- Remove one organism from the population
-- This kinda assumes the the given Population already has all possible keys..
popRemove :: Organism -> Population -> Population
popRemove organism population =
  Map.insertWith (-) organism 1 $ population


-- Probability of picking a list of organisms in order
prPickList :: [Organism] -> Population -> Float
prPickList [organism] population = prPickOne organism population
prPickList (organism:others) population =
  (prPickOne organism population) * (prPickList others $ popRemove organism population)


-- Probability of picking one organism from the given population
prPickOne :: Organism -> Population -> Float
prPickOne organism population =
  (popCount organism population) / (popTotal population)


main = do
  print $ popCount HomozygousRecessive $ newPopulation 1 2 3
  print $ popTotal $ newPopulation 1 2 3
  print $ popRemove HomozygousDominent $ newPopulation 1 2 3
  print $ prPickList [HomozygousDominent, HomozygousRecessive] (newPopulation 1 2 3)
