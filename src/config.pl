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
            game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces),
            Move = (Row-Column),
            get_el(Board, Row, Column, Element1),
            validate(Element1, ' ', Valid1), !,
            Piece = (OldRow-OldColumn),
            get_el(Board, OldRow, OldColumn, Element2),
            validate(Element2, Player, Valid2), !,
            Valid is Valid1 + Valid2,
            bfs([Piece], OldSize, Player, Board), !, 
            update_board(State, Move, Piece, NewState, Valid), 
            game_state_pack(NewState, NewBoard, Player_, Opponent_, RedPieces_, BluePieces_),
            bfs([Move], NewSize, Player, NewBoard), !,
            (
                NewSize =< OldSize ->
                update_board(State, Piece, Move, NewState, Valid),
                Valid = 0;
                Valid = 2
            ).

valid_move(State, NewState):-
        game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces),
        length(Board, Size),
        read_piece(Size, Piece), nl,
        read_move(Size, Move), nl, 
        check_valid_move(State, Piece, Move, NewState, Valid),
        Valid > 1,
        !.


valid_move(State, NewState):-
            write(' > Invalid move, try again!'), nl, nl, 
            valid_move(State, NewState).

% ---------- BOARD UPDATING ----------

update_board(State, Move, Piece, NewState, 0).
update_board(State, Move, Piece, NewState, 1).
update_board(State, Move, Piece, NewState, 2):- 
    Move = (Row-Column), 
    Piece = (OldRow-OldCol),
    game_state_pack(State, Board, CurrentPlayer, Opponent, RedPieces, BluePieces),
    place_piece(Row, Column, OldRow, OldCol, CurrentPlayer, Board, NewBoard),
    count_pieces(NewBoard, CurrentPlayer, CountCurPlayer),
    count_pieces(NewBoard, Opponent, CountOpponet),
    game_state_pack(NewState, NewBoard, Opponent, CurrentPlayer, CountCurPlayer, CountOpponet).

place_piece(Row, Column, OldRow, OldCol, Element, Board, NewBoard):-
        replace(Board, Row, Column, Element, TMP),
        replace(TMP, OldRow, OldCol, ' ', NewBoard).

% ---------- WINNING CONDITION ----------

winning_condition(State):-
        % bfs na ultima peca movida pelo jogador
        % checkar numero de peças do ultimo jogador que jogou com o res do bfs
        fail.

