module Generators (initialSequence, randomID) where

import Random
import Array exposing (Array)

type alias ID = Int

initialSequence : Random.Seed -> (Array ID, Random.Seed)
initialSequence seed =
  let
    listGenerator = Random.list 4 randomIDgenerator
    (list, newSeed) = Random.generate listGenerator seed
  in
    ( Array.fromList list , newSeed )


randomID : Random.Seed -> (ID, Random.Seed)
randomID seed =
  Random.generate randomIDgenerator seed


randomIDgenerator : Random.Generator ID
randomIDgenerator =
  Random.int 1 4
