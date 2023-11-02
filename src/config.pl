% Specify game configuration and settings here


:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(system), [now/1]).
:- consult(utils).
:- consult(data).

% --------- GRAPHIC PART ----------

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

bounce_game:-
     write('  ____                              \n'),                          
     write(' | __ )  ___  _   _ _ __   ___ ___  \n'),
     write(' |  _ \\ / _ \\| | | | |_ \\ / __/ _ \\ \n'),
     write(' | |_) | (_) | |_| | | | | (_|  __/ \n'),
     write(' |____/ \\___/ \\__,_|_| |_|\\___\\___| \n').

% ---------- MOVE VALIDATION ----------

validate(Rcvd, Expctd, Valid) :-
            (Rcvd == Expctd -> Valid = 1; Valid = 0).


check_valid_move(State, Piece, Move, NewState, Valid) :-
    game_state_pack(State, Board, Player, Opponent),
    Move = (Row-Column),
    get_el(Board, Row, Column, Element1),
    validate(Element1, ' ', Valid1),
    Piece = (OldRow-OldColumn),
    get_el(Board, OldRow, OldColumn, Element2),
    validate(Element2, Player, Valid2),
    dfs(Board, Row, Column, Player, [(Row, Column)], NewGroupSize), % Calculate the new group size
    dfs(Board, OldRow, OldColumn, Player, [(OldRow, OldColumn)], OldGroupSize), % Calculate the old group size
    (NewGroupSize > OldGroupSize -> Valid3 = 1; Valid3 = 0), % Check the group size condition
    Valid is Valid1 + Valid2 + Valid3,
    update_board(State, Move, Piece, NewState, Valid).


valid_move(State, NewState):-
            game_state_pack(State, Board, Player, Opponent),
            length(Board, Size),
            read_piece(Size, Piece), nl, 
            read_move(Size, Move), nl,
            check_valid_move(State, Piece, Move, NewState, Valid), 
            Valid > 2, 
            !.
          
valid_move(State, NewState):-
            write(' > Invalid move!'), nl, nl, 
            valid_move(State, NewState).

% ---------- BOARD UPDATING ----------

update_board(State, Move, Piece, NewState, 0).
update_board(State, Move, Piece, NewState, 1).
update_board(State, Move, Piece, NewState, 2).
update_board(State, Move, Piece, NewState, 3):- 
          Move = (Row-Column), 
          Piece = (OldRow-OldCol),
          game_state_pack(State, Board, CurrentPlayer, Opponent),
          place_piece(Row, Column, OldRow, OldCol, CurrentPlayer, Board, NewBoard),
          game_state_pack(NewState, NewBoard, Opponent, CurrentPlayer).

place_piece(Row, Column, OldRow, OldCol, Element, Board, NewBoard):-
        replace(Board, Row, Column, Element, TMP),
        replace(TMP, OldRow, OldCol, ' ', NewBoard).


% ---------- WINNING CONDITION ----------

winning_condition(State):-
          fail. % Por implementar


% ---------- DFS ----------
dfs(Board, Row, Col, Piece, Visited, GroupSize) :-
    valid_position(Board, Row, Col),                                        % ve se a posicao é valida
    get_el(Board, Row, Col, Element),                                       % pega no elemento da posicao
    Element \= ' ',                                                         % ve se é diferente de empty
    \+ member((Row, Col), Visited),                                         % verificar se nao esta na lista
    append(Visited, [(Row, Col)], NewVisited),                              % adiciona à lista
    find_neighboring_pieces(Board, Row, Col, Piece, NewVisited, NewGroup),  % procura nas pecas vizinhas
    length(NewGroup, NewGroupSize),                                         % ve o tamanho do grupo
    GroupSize is NewGroupSize + 1.                                          % +1 por causa da peça inicials

find_neighboring_pieces(_, _, _, _, [], []).
find_neighboring_pieces(Board, Row, Col, Piece, Visited, Group) :-
    neighbor_positions(Row, Col, NeighborRow, NeighborCol),
    dfs(Board, NeighborRow, NeighborCol, Visited, NewGroupSize),
    get_el(Board, NeighborRow, NeighborCol, NeighborPiece),
    NeighborPiece = Piece,
    append([(NeighborRow, NeighborCol)], NewGroup, Group).
find_neighboring_pieces(_, _, _, _, _, []).

neighbor_positions(Row, Col, NewRow, NewCol) :-
    neighbor_offsets(Offsets),
    member((OffsetRow, OffsetCol), Offsets),
    NewRow is Row + OffsetRow,
    NewCol is Col + OffsetCol.

neighbor_offsets([(1, 0), (-1, 0), (0, 1), (0, -1)]).  % Up, Down, Left, Right

valid_position(Board, Row, Col) :-
    length(Board, NumRows),
    length(Board, NumCols),
    Row >= 1, Row =< NumRows,
    Col >= 1, Col =< NumCols.