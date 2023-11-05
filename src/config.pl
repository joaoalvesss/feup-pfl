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
    game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
    Move = (Row-Column),
    get_el(Board, Row, Column, Element1),
    validate(Element1, ' ', Valid1), !,
    Piece = (OldRow-OldColumn),
    get_el(Board, OldRow, OldColumn, Element2),
    validate(Element2, Player, Valid2), !,
    Valid is Valid1 + Valid2,
    bfs([Piece], OldSize, Player, Board), !, 
    update_board(State, Move, Piece, NewState, Valid), 
    game_state_pack(NewState, NewBoard, Player_, Opponent_, RedPieces_, BluePieces_, Bot1_, Bot2_, Turn_),
    bfs([Move], NewSize, Player, NewBoard), !,
    %write(' > New Size: '), write(NewSize), nl,
    (
        NewSize =< OldSize ->
        update_board(State, Piece, Move, NewState, Valid),
        Valid = 0;
        get_win(Player, NewBoard, NewSize, Win),
        (Win == 1 -> winning_condition(NewState, Player); Valid1 = 1),
        Valid = 2
    ).


empty_list([], []).
valid_move(State, NewState):-
        game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
        length(Board, Size),
        (possible_move(Board, Player, Size, L1, L2)),
        \+ (empty_list(L1, L2)),
        write('List 1'), write(L1), nl,
        write('List 2'), write(L1), nl,
        write(' > There Is A Valid Move'), nl,
        read_piece(Size, Piece), nl,
        read_move(Size, Move), nl, 
        check_valid_move(State, Piece, Move, NewState, Valid),
        Valid > 1,
        !.

valid_move(State, NewState):-
        game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
        length(Board, Size),
        write(' > There is no valid move'), nl, 
        remove_piece(Board, NewBoard, Player),
        count_pieces(NewBoard, 'R', CountCurPlayer),
        count_pieces(NewBoard, 'B', CountOpponet),
        next_turn(Turn, NextTurn),
        game_state_pack(NewState, NewBoard, Opponent, Player, CountCurPlayer, CountOpponet, Bot1, Bot2, NextTurn), 
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
    game_state_pack(State, Board, CurrentPlayer, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
    place_piece(Row, Column, OldRow, OldCol, CurrentPlayer, Board, NewBoard),
    count_pieces(NewBoard, CurrentPlayer, CountCurPlayer),
    count_pieces(NewBoard, Opponent, CountOpponet),     %possivel erro na linha de baixo
    next_turn(Turn, NextTurn),
    game_state_pack(NewState, NewBoard, Opponent, CurrentPlayer, CountCurPlayer, CountOpponet, Bot1, Bot2, NextTurn). 

place_piece(Row, Column, OldRow, OldCol, Element, Board, NewBoard):-
        replace(Board, Row, Column, Element, TMP),
        replace(TMP, OldRow, OldCol, ' ', NewBoard).

% ---------- WINNING CONDITION ----------


get_win(Player, Board, NewSize, Win):-
    count_pieces(Board, Player, N),
    write('Counted '), write(N), nl,
    write('New Size '), write(NewSize), nl,
    win(Player, N, NewSize, Win).

win('R', RedPieces, NewSize, Win):-
    NewSize >= RedPieces,
    Win is 1, !.

win('R', RedPieces, NewSize, Win):-
    Win is 0.

win('B', BluePieces, NewSize, Win):-
    NewSize >= BluePieces,
    Win is 1, !.

win('B', BluePieces, NewSize, Win):-
    Win is 0.

winning_condition(State, Player):-
    %clear_console, 
    nl, nl,
    game_state_pack(State, Board, CurrentPlayer, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
    display_game(Board),
    (
        Player = 'R' -> red_wins;
        Player = 'B' -> blue_wins
    ), 
    nl, write(' > Enter "play." to start a new game or "exit." to quit: '), nl, 
    read(PlayerChoice),
    write(PlayerChoice),
    handle_user_choice(PlayerChoice).

handle_user_choice('play'):- 
    play.

handle_user_choice('exit'):- 
    write(' > Thanks for playing! Goodbye.'), halt.

handle_user_choice(Other) :- 
    nl, write(' > Invalid choice. Enter "play." to start a new game or "exit." to quit: '),
    read(NewChoice),
    handle_user_choice(NewChoice).


