% Specify game configuration and settings here

:- consult(config).
:- consult(board).
:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).


% Choose Bot difficulty (1 or 2)
choose_difficulty(Bot) :-
    format('Please select a bot difficulty:\n', [Bot]),
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    get_option(1, 2, 'Difficulty', Option), !,
    asserta((difficulty(Bot, Option))).


% Main menu options. Each represents a game mode.
option(1):-
    write('Human vs. Human\n'),
    get_name(player1), get_name(player2).
option(2):-
    write('Human vs. Bot\n'),
    get_name(player1),
    asserta((name_of(player2, 'bot'))), !, 
    choose_difficulty(player2).
option(3):-
    write('Bot vs. Bot\n'),
    asserta((name_of(player1, 'bot1'))),
    asserta((name_of(player2, 'bot2'))), !,
    choose_difficulty(player1),
    choose_difficulty(player2).


% Unifies Player with the red player (player1)
choose_player(Player):-
    name_of(player1, Player).


% Game header
bounce:-
    write('   _____ _                 _    \n'),
    write('  / ____| |               | |   \n'),
    write(' | |    | |__   __ _ _ __ | | __\n'),
    write(' | |    | \'_ \\ / _` | \'_ \\| |/ /\n'),
    write(' | |____| | | | (_| | | | |   < \n'),
    write('  \\_____|_| |_|\\__,_|_| |_|_|\\_\\\n'),
    write('\n'),
    write('=================================\n'),
    write('   Welcome to Bounce!   \n'),
    write('=================================\n').

% Main menu
menu:-  
    write('===================================\n'),
    write('         Bounce Game Menu          \n'),
    write('===================================\n\n'),
    write('Please select a game mode:\n'),
    write('  1 - Human vs. Human\n'),
    write('  2 - Human vs. Bot\n'),
    write('  3 - Bot vs. Bot\n\n').


% Game mode choice
set_mode :-
    menu,
    get_option(1, 3, 'Mode', Option), !,
    option(Option).


% Board size choice
choose_board(Size):-
    write('Board size (6, 8 or 10):  '),
    repeat,
    read_number(Size),
    Size > 0,
    0 is Size mod 2, !.


% Initialize GameState with the board, first Player, empty group data, and total moves
configurations([Board,Player,[],0]):-
    bounce,
    set_mode,
    init_random_state,
    choose_player(Player),
    choose_board(Size), 
    init_state(Size, Board).
