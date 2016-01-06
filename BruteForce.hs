-- | the module from which the brute-forcing function is imported
module BruteForce (bruteForce, bruteForce') where

import           Control.Monad (replicateM)

-- | the real heart of the program: lists every possible combination
-- from an alphabet given that and a combination length
bruteForce'
  :: [a]   -- ^ an input alphabet
  -> Int   -- ^ the list length
  -> [[a]] -- ^ the combination list
bruteForce' alphabet = flip replicateM alphabet

-- | the heart of the program: lists every possible combination
bruteForce
  :: [a]    -- ^ an 'alphabet' (list to choose elements from)
  -> [[a]]  -- ^ stream of all possible lists
bruteForce alphabet = concatMap (bruteForce' alphabet) [1..]
