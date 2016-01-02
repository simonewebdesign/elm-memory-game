module Generators (initialSequence, randomID) where

import Random
import Array exposing (Array)

type alias ID = Int

initialSequence : Array ID
initialSequence =
  let
    listGenerator = Random.list 4 randomIDgenerator
    seed = Random.initialSeed 123 -- not random, always generates [2,4,2,4]
    (list, _) = Random.generate listGenerator seed
  in
    Array.fromList list


randomID : Random.Seed -> (ID, Random.Seed)
randomID seed =
  Random.generate randomIDgenerator seed


randomIDgenerator : Random.Generator ID
randomIDgenerator =
  Random.int 1 4

  --let
  --  (id, newSeed) = randomId model.seed
  --in
  --Array.repeat 4 randomID
  --Array.foldl randomID Array.empty
  --Array.foldl (\elem acc ->
  --  let
  --    (id, newSeed) = randomId
  --) Array.empty


--pop : Array a -> a
--pop arr =
--  let [value] = Array.slice 0 -1 arr
--  in value
-- Returns the array with only the last button.
--last : Array a -> Array a
--last a =
--  Array.slice -1 (Array.length a) a
