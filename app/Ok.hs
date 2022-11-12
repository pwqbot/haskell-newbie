module Ok where

import Data.List
-- import Data.Map qualified as M

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub



-- map sum $ transpose [[0, 3, 5], [2, 6, 1], [8, 0, 9]]
