---
title: Coding a Moderately Complex Game in Haskell and Elm
author: Arnaud Bailly 
date: 2016-10-16
---

In 2015, I started coding an Haskell implementation of [Acquire](), an old boardgame I liked very much back in the days when I had time to play physical boardgames with friends. The initial goal was exploring how Haskell can help write such games, especially when it comes to *mapping*  the rules to actual game: What's the overhead of writing non ambiguous rules to be interpreted by a computer, how Haskell's emphasis on pure lazy functional programming helps (or not) expressing those rules and translate them to types and expressions. Then of course it became interesting to actually play the game, possibly against other players, so I wrote a simple command-line based "shell" that allows to run games in client-server mode using only CLI. Finally, when I became interested in [Elm](http://elm-lang.org) in mid-2016, I thought it would be a good idea to write an Elm client to provide a nicer interface and to play with WebSockets in both Elm and Haskell. 

This blog post is a summary of this experience that I hope will be useful for other people wanting to explore how to program using 2 different but similar pure functional languages. 

# The Game

I won't say much about the details of the game itself as this is not what interested me. Detailed rules can be found [here](http://www.webnoir.com/bob/sid/acquire.htm) or of course in a printed copy of the game. High-level  characteristics of Acquire as a game are:

* It is a turn-based game: Play proceeds which each player doing some action in a fixed sequence of turns,
* Sequencing of actions can be complex: When mergers occur, possible actions deviate from standard course of the game which creates richer interactions and more complex decisions,
* It contains some randomness: Tiles are drawn at random from a set of available tiles,
* Information is partial: The set of playable tiles for each player is hidden, as well as the shares they owned,
* It is fast-paced: A game can be played in about an hour, depending on speed and number of players.

Players can be either humans connecting to the server through CLI or Web UI, or robots. Robots are unsophisticated: They play randomly one move from all the possible moves according to current game state.

Winner of the game can only be really known when game ends as it depends on valuation of companies' shares held.

# Core Game & Server

This part is written in Haskell and is conceptually separated in three components: 

* Core game handling: 100% pure and encodes the game's rules and logic using a bunch of functions and data types,
* Players' interaction: Handles I/O locally, with players and to store/load game state from persistent storage. This part is monadic and is based on the [Prompt](http://joeysmandatory.blogspot.com/2012/06/explaining-prompt-monad-with-simpler.html) monad,
* Web server: Serves game over HTTP, managing interactions using [WebSockets](https://developer.mozilla.org/en-US/docs/Web/API/WebSockets_API).

## Pure Game Logic

The game's logic is encapsulated in a couple of functions: 

* A function that generates the possible `Order`s given some `Game` state,

    ```.haskell
    possiblePlay :: Game -> [ Order ]
    ```
    
* A function that *applies* some `Order` to a `Game`, producing a new `Game`:

    ```.haskell
    play :: Game -> Order -> Game
    ```
    
Hence playing a game amounts to folding sequence of calls to `possiblePlay` and `play` over a starting game. Thanks to Haskell's laziness, it is event possible to "generate" lazily the game tree which opens up a whole world of possibilities for coding smarter bots.


