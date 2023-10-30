% Represent the game data, such as the game state here

% Find the Players name
:- dynamic name_of/2.


% Find the Bot difficulty
:- dynamic difficulty/2.


% Board structure
board(6, [
        [empty, blue, red, blue, red, empty],
        [blue, red, blue, red, blue, red],
        [red, blue, red, blue, red, blue],
        [blue, red, blue, red, blue, red],
        [red, blue, red, blue, red, blue],
        [empty, red, blue, red, blue, empty].

]).

board(8, [
        [empty, blue, red, blue, red, blue, red, empty],
        [blue, red, blue, red, blue, red, blue, red],
        [red, blue, red, blue, red, blue, red, blue],
        [blue, red, blue, red, blue, red, blue, red],
        [red, blue, red, blue, red, blue, red, blue],
        [blue, red, blue, red, blue, red, blue, red],
        [red, blue, red, blue, red, blue, red, blue],
        [empty, red, blue, red, blue, red, blue, empty].
]).

board(10, [
        [empty, blue, red, blue, red, blue, red, blue, red, empty],
        [blue, red, blue, red, blue, red, blue, red, blue, red],
        [red, blue, red, blue, red, blue, red, blue, red, blue],
        [blue, red, blue, red, blue, red, blue, red, blue, red],
        [red, blue, red, blue, red, blue, red, blue, red, blue],
        [blue, red, blue, red, blue, red, blue, red, blue, red],
        [red, blue, red, blue, red, blue, red, blue, red, blue],
        [empty, red, blue, red, blue, red, blue, red, blueempty].
]).

other_player(player1, player2).
other_player(player2, player1).
