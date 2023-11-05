# Bounce

## Introduction
Bounce is a captivating two-player strategy game, designed by Mark Steere in August 2023. Played on a square board of any even size, Bounce offers a unique and challenging gaming experience. This repository contains a Prolog implementation of the game.

## Group
- **Group members and contribution:**
  - João Brandão Alves up202108670 - 50%
  - Diogo Silva Leandro up202005304 - 50%

- **Group identification:**
  - Bounce_2

## Installation and Execution
To play the game you first need to have SICStus Prolog 4.8 or a newer version currently installed in your machine plus the folder with the source code. 

Next, on the SICStus interpreter, consult the file *main.pl* located in the source root directory:

    ?- consult('./main.pl').

If you're using Windows, you can also do this by selecting `File` -> `Consult...` and selecting the file `main.pl`.
    
Finally, run the the predicate play/0 to enter the game main menu: 

    ?- play.

The game is best enjoyed by selecting a bold and impactful font into SicStus' terminal.

## Description of the Game
Bounce is a two-player board game played on a square board of any even size. The objective is to have all of your checkers in one group at the end of your turn to win.

### Official Rule Sheet
For detailed game rules, you can refer to the [official rule sheet](https://marksteeregames.com/Bounce_rules.pdf) provided by Mark Steere.

## Game Logic

### Internal Game State Representation
The game is played on a square board of any even size, initially filled with a checkerboard pattern of red and blue checkers. The game's internal state is represented as follows:

- `Board`: A square matrix representing the game board.
- `CurrentPlayer`: The player whose turn it is (either 'R' for Red or 'B' for Blue).
- `Opponent`: The other player (the opponent).
- `RedPieces`: The number of red checkers on the board.
- `BluePieces`: The number of blue checkers on the board.

### Game State Visualization
The game state can be visually represented using the `display_game/1` predicate, which displays the current state of the board. This allows players to see the current configuration of the board.

### Move Validation and Execution
The game enforces rules for valid moves and executes them accordingly. A move is considered valid if it meets the following criteria:
- The destination square is unoccupied.
- The checker being moved belongs to the player whose turn it is.
- The move results in an increase in the size of the group the checker belongs to.

If a move is invalid, the player is prompted to try again. If there are no valid moves, the player must remove one of their checkers from the board.

### List of Valid Moves
The game checks for valid moves by evaluating each possible move on the board, considering the game's rules. The `valid_move/2` predicate interacts with the player to validate and execute a move.

### End of Game
The game ends when a player successfully merges all their checkers into one group, making them the winner. The game then provides an option for the player to start a new game or exit.

### Game State Evaluation
The `bfs/5` predicate performs breadth-first search to evaluate the size of interconnected groups on the board. This information is used to check if a move increases the size of a group, and whether the player has won by merging all their checkers into one group.

### Computer Plays
**The game is designed for two human players. No computer-based player (AI) is implemented in this version.**

## Conclusions
The "Bounce" game is a strategy board game that provides an engaging experience for two players. By following the rules and making strategic moves, players aim to merge their checkers into a single group to secure a victory. The provided Prolog code offers the foundation for playing the game and managing its rules, but it does not include a computer-based opponent.

## Bibliography
The "Bounce" game was designed by Mark Steere in August 2023.