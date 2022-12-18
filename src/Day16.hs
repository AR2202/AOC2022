module Day16
  ( maxPressure
  , exampleflows
  , exampletunnel
  ) where

import           Common

import qualified Data.List as L
import qualified Data.Map  as M

type Tunnel = M.Map String [String]

type Flows = M.Map String Int

maxPressure 0 tunnel flows open currentnode = 0
maxPressure timeRemaining tunnel flows open currentnode =
  maximum $
  (map
     (maxPressure (timeRemaining - 1) tunnel flows open)
     ((M.findWithDefault [] currentnode tunnel) L.\\ open) ++
   map
     ((+ ((timeRemaining - 1) * (M.findWithDefault 0 currentnode flows))) .
      maxPressure (timeRemaining - 2) tunnel flows (currentnode : open))
     ((M.findWithDefault [] currentnode tunnel) L.\\ open))

exampleflows =
  M.fromList
    [ ("AA", 0)
    , ("BB", 13)
    , ("CC", 2)
    , ("DD", 20)
    , ("EE", 3)
    , ("FF", 0)
    , ("GG", 0)
    , ("HH", 22)
    , ("II", 0)
    , ("JJ", 21)
    ]

exampletunnel =
  M.fromList
    [ ("AA", ["DD", "II", "BB"])
    , ("BB", ["CC", "AA"])
    , ("CC", ["DD", "BB"])
    , ("DD", ["CC", "AA", "EE"])
    , ("EE", ["FF", "DD"])
    , ("FF", ["EE", "GG"])
    , ("GG", ["FF", "HH"])
    , ("HH", ["GG"])
    , ("II", ["AA", "JJ"])
    , ("JJ", ["II"])
    ]
