% Represent the game data, such as the game state here
:- dynamic name_of/2.
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
