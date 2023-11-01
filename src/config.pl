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



validate(Rcvd, Expctd, Valid) :-
    (Rcvd == Expctd -> Valid = 1; Valid = 0).


check_valid_move(State, Piece, Move, NewState, Valid) :-
    game_state_pack(State, Board, Player, Opponent),
    Move = (Row-Column),
    get_el(Board, Row, Column, Element1),
    %write('Element_1: '), write(Element1), nl,
    validate(Element1, ' ', Valid1),
    Piece = (OldRow-OldColumn),
    get_el(Board, OldRow, OldColumn, Element2),
    %write('Element_2: '), write(Element2), nl,
    validate(Element2, Player, Valid2),
    Valid is Valid1 + Valid2,
    update_board(State, Move, Piece, NewState, Valid).



valid_move(State, NewState):-
          game_state_pack(State, Board, Player, Opponent),
          length(Board, Size),
          %write(' > Which piece to move? \n'),
          read_piece(Size, Piece), nl, 
          %write(' > Where to move the piece? \n'),
          read_move(Size, Move), nl,
          check_valid_move(State, Piece, Move, NewState, Valid), 
          %write('Valid = '), write(Valid), nl,
          Valid > 1,
          !.
          
valid_move(State, NewState):-
          write(' > Invalid move!\n'), nl,
          valid_move(State, NewState)
          .
update_board(State, Move, Piece, NewState, 0).
update_board(State, Move, Piece, NewState, 1).
update_board(State, Move, Piece, NewState, 2):- 
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



% ---------- DFS ----------
dfs(Board, Row, Col, Visited, Cluster) :-
    valid_position(Board, Row, Col),
    get_element(Board, Row, Col, Piece),
    Piece \= ' ',
    \+ member((Row, Col), Visited),
    append(Visited, [(Row, Col)], NewVisited),
    find_neighboring_pieces(Board, Row, Col, Piece, NewVisited, NewCluster),
    append(Cluster, NewCluster, Cluster).


find_neighboring_pieces(_, _, _, _, [], []).
find_neighboring_pieces(Board, Row, Col, Piece, Visited, Cluster) :-
    neighbor_positions(Row, Col, NeighborRow, NeighborCol),
    dfs(Board, NeighborRow, NeighborCol, Visited, NewCluster),
    get_element(Board, NeighborRow, NeighborCol, NeighborPiece),
    NeighborPiece = Piece,
    append([(NeighborRow, NeighborCol)], NewCluster, Cluster).
find_neighboring_pieces(_, _, _, _, _, []).


neighbor_positions(Row, Col, NewRow, NewCol) :-
    neighbor_offsets(Offsets),
    member((OffsetRow, OffsetCol), Offsets),
    NewRow is Row + OffsetRow,
    NewCol is Col + OffsetCol.

neighbor_offsets([(1, 0), (-1, 0), (0, 1), (0, -1)]).  % Up, Down, Left, Right


get_element(Board, Row, Col, Element) :-
    nth0(Row, Board, RowList),
    nth0(Col, RowList, Element).

valid_position(Board, Row, Col) :-
    length(Board, NumRows),
    length(Board, NumCols),
    Row >= 0, Row < NumRows,
    Col >= 0, Col < NumCols.