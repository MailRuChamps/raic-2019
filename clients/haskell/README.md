# Haskell for Russian AI Cup 2019

Implementation of Haskell language package for RAIC.
Serialization is made with package `binary`. 
Improvements and contributions are welcome

## Structure
RAIC - is root directory of the package
* Model - contain models for the game
    * A lot of game models...
* Utils - helper module
    * Trans - declares `Trans` typeclass for custom serialization, contain 
    instances for primitives
    * TCPSocket - Functionality to connect to network socket
    * StreamWrapper - wrap around `io-streams` lib to read/write `ByteString`'s
* Main - connect to socket and start game loop
* MyStrategy - define Player's strategy

### TODO:
* Implement default strategy in `getAction`
* Output debug messages from `getAction` function
* refactor `Model`'s names
* Consider adding `lens` support
