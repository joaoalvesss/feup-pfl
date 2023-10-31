% Represent the game data, such as the game state here

% Find the Players name
:- dynamic name_of/2.

% Find the Bot difficulty
:- dynamic difficulty/2.


% Board structure
board1(4, [
        [' ', 'B', 'R', ' '],
        ['B', 'R', 'B', 'R'],
        ['R', 'B', 'R', 'B'],
        [' ', 'R', 'B', ' ']

]).

board2(6, [
        [' ', 'B', 'R', 'B', 'R', ' '],
        ['B', 'R', 'B', 'R', 'B', 'R'],
        ['R', 'B', 'R', 'B', 'R', 'B'],
        ['B', 'R', 'B', 'R', 'B', 'R'],
        ['R', 'B', 'R', 'B', 'R', 'B'],
        [' ', 'R', 'B', 'R', 'B', ' ']

]).

board3(8, [
        [' ', 'B', 'R', 'B', 'R', 'B', 'R', ' '],
        ['B', 'R', 'B', 'R', 'B', 'R', 'B', 'R'],
        ['R', 'B', 'R', 'B', 'R', 'B', 'R', 'B'],
        ['B', 'R', 'B', 'R', 'B', 'R', 'B', 'R'],
        ['R', 'B', 'R', 'B', 'R', 'B', 'R', 'B'],
        ['B', 'R', 'B', 'R', 'B', 'R', 'B', 'R'],
        ['R', 'B', 'R', 'B', 'R', 'B', 'R', 'B'],
        [' ', 'R', 'B', 'R', 'B', 'R', 'B', ' ']
]).

piece_info('R', player1).
piece_info('B', player2).
piece_info(' ', neutral).

other_player(player1, player2).
other_player(player2, player1).
