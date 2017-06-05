module Scratchpad where

import Prelude
import Data.Generic

data Food = Bread | Cheese

data Happiness = Happy | Neutral | Unhappy
instance showHappiness :: Show Happiness where
  show Happy = "Happy"
  show Neutral = "Neutral"
  show Unhappy = "Unhappy"

murph :: Food -> Happiness
murph Bread = Unhappy
murph Cheese = Happy

data NPC = Ogre { name :: String, strength :: Int }
         | Wolf { name :: String, strength :: Int }

derive instance genericNPC :: Generic NPC
instance showNPC :: Show NPC where
  show = gShow

changeName :: NPC -> NPC
changeName (Ogre r) = Ogre r { name = "Shrek" }
changeName (Wolf r) = Wolf r { name = "Big Bad" }

getFullName :: String -> String -> String
getFullName first last = first <> " " <> last

type First = String
type Last = String
type Full = String
getFullNameSemantic :: First -> Last -> Full
getFullNameSemantic first last = first <> " " <> last

derive instance genericFull :: Generic Full'
instance showFull :: Show Full' where
  show = gShow

newtype First' = First String
newtype Last' = Last String
newtype Full' = Full String
getFullNameEnforced :: First' -> Last' -> Full'
getFullNameEnforced (First f) (Last l) = Full $ f <> " " <> l