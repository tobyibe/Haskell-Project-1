module Actions where

import World

actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions _         = Nothing

commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Nothing

{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: String -> Room -> Maybe String
move dir rm = foldr (\x xs -> if dir == (exit_dir x) then Just (room x) else xs) Nothing (exits rm)


{- Return True if the object appears in the room. -}
-- ran with objectHere "mug" kitchen
objectHere :: String -> Room -> Bool
objectHere o rm = foldr (\x xs -> if (o == (obj_name x)) then True else xs)False (objects rm)

{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: String -> Room -> Room
removeObject o rm = Room {room_desc = room_desc rm, exits = exits rm, objects = filter (\x -> obj_name x /= o) (objects rm)}


{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = Room {room_desc = (room_desc rm), exits = (exits rm), objects = [o]++(objects rm)}

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}

findObj :: String -> [Object] -> Object
findObj name (x:xs) | name == (obj_name x) = x
                    | otherwise = findObj name xs

{- Use 'findObj' to find an object in a room description -}

objectData :: String -> Room -> Object
objectData o rm = findObj o (objects rm)

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata = gd { world = updatedWorld }
    where
        updatedWorld = map updateRoomIfMatch (world gd)
        updateRoomIfMatch (rid, r)
            | rid == rmid = (rmid, rmdata)
            | otherwise   = (rid, r)

-- | Given a game state and an object name, find the object in the current
-- | room and add it to the player's inventory.
addInv :: GameData -> String -> GameData
addInv gd objName
    | objectHere objName currentRoom = gd { inventory = object : inventory gd }
    | otherwise = gd  -- Object not in the room, no change to game state
    where
      currentRoom = getRoomData gd  -- Get the current room data from the game state
      object = objectData objName currentRoom  -- Get the object data using objectData function


{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

-- | Removes an object from the player's inventory.
removeInv :: GameData -> String -> GameData
removeInv gd objName = gd { inventory = filter (\obj -> obj_name obj /= objName) (inventory gd) }

{- Does the inventory in the game state contain the given object? -}
carrying :: GameData -> String -> Bool
carrying gd obj = 
   let list = (inventory gd)
   in case list of 
      [] -> False
      (x:xs) -> if (obj_name x == obj) then True else carrying (removeInv gd (obj_name x)) obj  
{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Action
go dir state =
   case move dir (getRoomData state) of
        Just newRoomId ->
            (state {location_id=newRoomId}, "OK\n")
        Nothing ->
            (state , "Can't go that way.\n")

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}

get :: Action
get obj state = 
   if objectHere obj (getRoomData state)
        then let
                 updatedState = addInv state obj
                 updatedRoom = removeObject obj (getRoomData state)
                 finalState = updateRoom updatedState (location_id state) updatedRoom
             in (finalState, "You picked up: " ++ obj ++ ".")
        else (state, "The object: " ++obj++ " is not in the room.")

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

put :: Action
put obj state = undefined
   {-if carrying state obj   WORKING ON IT - cris
        then let
                 updatedState = removeInv state obj
                 updatedRoom = addObject (findObj obj) (getRoomData state)
                 finalState = updateRoom updatedState (location_id state) updatedRoom
             in (finalState, "You dropped: " ++ obj ++ ".")
        else (state, "The object: " ++obj++ " is not in your inventory.")-}

{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action
examine obj state = undefined

{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Action
pour obj state = undefined

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Action
drink obj state = undefined

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: Action
open obj state = undefined

{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_name x
         showInv' (x:xs) = obj_name x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")

