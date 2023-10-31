% Specify game configuration and settings here


:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(data).

% Game header
bounce:-
     write(' ____                              \n'),                          
     write('| __ )  ___  _   _ _ __   ___ ___  \n'),
     write('|  _ \\ / _ \\| | | | |_ \\ / __/ _ \\ \n'),
     write('| |_) | (_) | |_| | | | | (_|  __/ \n'),
     write('|____/ \\___/ \\__,_|_| |_|\\___\\___| \n'),
     write('\n'),
     write('---------------------------------\n'),
     write('|            Welcome            |\n'),
     write('|         To Game Menu!         |\n'),
     write('---------------------------------\n').
     %write('> Please select a game mode:\n'),
     %write('  1 - 6x6 game\n'),
     %write('  2 - 8x8 game\n'),
     %write('  3 - 10x10 game\n\n').


valid_move(State, NewState):-
          fail. % Por implementar

check_valid_move(State, Move):-
          fail. % Por implementar.

update_board(State, Move, NewState):- 
          fail. % Por implementar

winning_condition(State):-
          fail. % Por implementar

