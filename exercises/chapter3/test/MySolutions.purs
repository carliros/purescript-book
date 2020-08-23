module Test.MySolutions where

import Prelude

import Data.AddressBook (Entry)
import Data.List (List, find, nubBy)
import Data.Maybe (Maybe, isJust)

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> List Entry -> Maybe Entry
findEntryByStreet street = find isStreet
    where isStreet entry = entry.address.street == street

isInBook :: String -> String -> List Entry -> Boolean
isInBook firstName lastName = isJust <<< find matchFullName
    where matchFullName entry = entry.firstName == firstName && entry.lastName == lastName

removeDuplicates :: List Entry -> List Entry
removeDuplicates = nubBy isDuplicate
    where isDuplicate first second =  first.firstName == second.firstName 
                                   && first.lastName == second.lastName