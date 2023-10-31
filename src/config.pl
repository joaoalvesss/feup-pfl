% Specify game configuration and settings here


:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(data).

% Game header
bounce:-
     write('  ____                              \n'),                          
     write(' | __ )  ___  _   _ _ __   ___ ___  \n'),
     write(' |  _ \\ / _ \\| | | | |_ \\ / __/ _ \\ \n'),
     write(' | |_) | (_) | |_| | | | | (_|  __/ \n'),
     write(' |____/ \\___/ \\__,_|_| |_|\\___\\___| \n'),
     write('\n'),
     write(' ---------------------------------\n'),
     write(' |            Welcome            |\n'),
     write(' |         To Game Menu!         |\n'),
     write(' ---------------------------------\n').


valid_move(State, NewState):-
          game_state_pack(State, Board, Player, Opponent),
          length(Board, Size),
          write(' > Which piece to move? \n'),
          read_piece(Size, Piece), nl, 
          write(' > Where to move the piece? \n'),
          read_move(Size, Move), nl,
          %(check_valid_move(State, Piece, Move) ->
          %    update_board(State, Move, NewState)
          %;
          %write(' > Invalid move!\n'), nl,
          %valid_move(State, NewState)
          %)
          update_board(State, Move, Piece, NewState) %test only  
          .


check_valid_move(State, Piece, Move):-
          game_state_pack(State, Board, Player, Opponent),
          Move = (Row-Column),  
          nth0(Row, Board, BoardRow),
          nth0(BoardColumn, BoardRow, Element),
          %write('Element: \''), %write(Element), %write('\''), nl,
          Element == ' '.

update_board(State, Move, Piece, NewState):- 
          Move = (Row-Column), 
          Piece = (OldRow-OldCol),
          game_state_pack(State, Board, CurrentPlayer, Opponent),
          place_piece(Row, Column, OldRow, OldCol, CurrentPlayer, Board, NewBoard),
          game_state_pack(NewState, NewBoard, Opponent, CurrentPlayer).

place_piece(Row, Column, OldRow, OldCol, Element, Board, NewBoard):-
        replace(Board, Row, Column, Element, TMP),
        replace(TMP, OldRow, OldCol, ' ', NewBoard).

winning_condition(State):-
          fail. % Por implementar

% group_check(Board, Player, Group)
% Group is a list of coordinates representing a connected group of checkers belonging to Player
% Board is the game board

group_check(Board, Player, Group) :-
    find_starting_point(Board, Player, StartX, StartY),
    (StartX >= 0, StartY >= 0 ->
        dfs(Board, Player, StartX, StartY, [], Group)
    ;   Group = []
    ).

% Find a starting point for the DFS search
find_starting_point(Board, Player, X, Y) :-
    length(Board, Size),
    between(0, Size-1, X),
    between(0, Size-1, Y),
    get_element(X, Y, Board, Player).

% Depth-First Search to find a connected group
dfs(_, _, X, Y, Visited, Group) :-
    \+ member((X, Y), Visited),  % Avoid revisiting already visited squares
    get_element(X, Y, Board, Player),
    Player \= ' ',  % Not an empty square
    append(Visited, [(X, Y)], NewVisited),
    find_neighbours(X, Y, Neighbours),
    find_connected_neighbours(Board, Player, NewVisited, Neighbours, NewGroup),
    append(NewGroup, [(X, Y)], Group).

% Find connected neighbours of the same Player
find_connected_neighbours(_, _, Visited, [], Visited).
find_connected_neighbours(Board, Player, Visited, [(X, Y) | Rest], Group) :-
    (X >= 0, Y >= 0, X < Size, Y < Size, \+ member((X, Y), Visited) ->
        get_element(X, Y, Board, Player),
        append(Visited, [(X, Y)], NewVisited),
        find_neighbours(X, Y, Neighbours),
        find_connected_neighbours(Board, Player, NewVisited, Neighbours, NewGroup),
        append(NewGroup, [(X, Y)], Group)
    ;   find_connected_neighbours(Board, Player, Visited, Rest, Group)
    ).

% Define valid neighbours
find_neighbours(X, Y, Neighbours) :-
    Neighbours = [(X-1, Y), (X+1, Y), (X, Y-1), (X, Y+1)].