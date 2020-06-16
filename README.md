# Adventure Game in Haskell
- Author: Byung Eun Jeon
- Project for Functional Programming course at the University of Chicago

![game-start-screenshot](https://github.com/byungeunjeon/functional-adventure/blob/master/screenshot.png)

## Prerequisites and Usage

Haskell GHC version 8.8.3
```
stack build
stack exec functional-adventure
```

## Command

You can use these commands
- look
- take (item)
- drop (item)
- north
- west
- south
- east
- (command) and (command) and ...
- exit
- quit

## Hint for Winning

Since the player is lost in an island, the winning strategy is to bring a certain item into somewhere so that the player can be rescued. Be aware that there is a limit to the total weight of items that players can carry. 

## Game Over

Player can die, ending the game prematurely. This happens when the player takes a ball (which is actually a grenade). The item "ball" is located in two places: garage and living room. Taking any one of two balls will make the player die. 

For example, you can do this from the initialState to make player die. 
```
-> north and take ball
```
