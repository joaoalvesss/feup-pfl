:- consult(utils).


config_bot(1, Bot1, Bot2):-
    Bot1 = 0,
    Bot2 = 0.

config_bot(2, Bot1, Bot2):-
    write('Set bot difficulty : "1."Easy / "2."Hard : '), nl,
    read(Bot1),
    Bot2 = 0.

config_bot(3, Bot1, Bot2):-
    write('Set bot 1 difficulty : "1."Easy / "2."Hard : '), nl,
    read(Bot1),
    write('Set bot 2 difficulty : "1."Easy / "2."Hard : '), nl,
    read(Bot2).




bot_move(State, NewState, BotDif):-
    game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
    length(Board, Size),
    possible_move(Board, Player, Size, LPieces, LMoves),        %missing not possible
    choose_move(BotDif, LPieces, LMoves, Piece, Move, Board),
    write(' > Piece : '), write(Piece), write(' - > '),
    write('Move : '), write(Move), nl,
    update_board(State, Move, Piece, NewState, 2).




choose_move(BotDif, [HeadPieces|LPieces], [HeadMoves|LMoves], Piece, Move, Board):-
    BotDif == 1,
    Piece = HeadPieces,
    Move = HeadMoves,
    !.


choose_move(BotDif, [HeadPieces|LPieces], [HeadMoves|LMoves], Piece, Move, Board):-
    BotDif == 2,
    choose_move(BotDif, [HeadPieces|LPieces], [HeadMoves|LMoves], Piece, Move, 0, CurBestPiece, CurBestMove, Board).


choose_move(BotDif, [], [], Piece, Move, BestSize, CurBestPiece, CurBestMove, Board):-
    Piece = CurBestPiece,
    Move = CurBestMove.

choose_move(BotDif, [HeadPieces|LPieces], [HeadMoves|LMoves], Piece, Move, BestSize, CurBestPiece, CurBestMove, Board):-
    HeadMoves = (Row-Column),
    HeadPieces = (OldRow-OldColumn),
    get_el(Board, OldRow, OldColumn, Player),
    place_piece(Row, Column, OldRow, OldColumn, Player, Board, TmpBoard),
    bfs([HeadMoves], NewSize, Player, TmpBoard),
    NewSize > BestSize,
    NCurBestPiece = HeadPieces,
    NCurBestMove = HeadMoves,
    NBestSize = NewSize,
    choose_move(BotDif, LPieces, LMoves, Piece, Move, NBestSize, NCurBestPiece, NCurBestMove, Board),
    !.


choose_move(BotDif, [HeadPieces|LPieces], [HeadMoves|LMoves], Piece, Move, BestSize, CurBestPiece, CurBestMove, Board):-
    choose_move(BotDif, LPieces, LMoves, Piece, Move, BestSize, CurBestPiece, CurBestMove, Board),
    !.
