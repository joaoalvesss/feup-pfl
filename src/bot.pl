:- consult(utils).

% config_bot(Option, Bot1, Bot2)
% Setups the bot according to the user specification
config_bot(1, Bot1, Bot2):-
    Bot1 = 0,
    Bot2 = 0.

config_bot(2, Bot1, Bot2):-
    write(' > Set bot difficulty : "1."Easy / "2."Hard : '),
    read(Bot1),
    nl,
    Bot2 = 0.

config_bot(3, Bot1, Bot2):-
    write(' > Set bot 1 difficulty : "1."Easy / "2."Hard : '),
    read(Bot1),
    nl,
    write(' > Set bot 2 difficulty : "1."Easy / "2."Hard : '),
    read(Bot2),
    nl.


% bot_move(State, NewState, BotDif)
% Creates and processes the next bot move 
bot_move(State, NewState, BotDif):-
    game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
    length(Board, Size),
    possible_move(Board, Player, Size, LPieces, LMoves),        
    choose_move(BotDif, LPieces, LMoves, Piece, Move, Board),
    update_board(State, Move, Piece, NewState, 2),
    game_state_pack(NewState, NewBoard, Player_, Opponent_, RedPieces_, BluePieces_, Bot1_, Bot2_, Turn_),
    bfs([Move], NewSize, Player, NewBoard), !,
    get_win(Player, NewBoard, NewSize, Win),
        (Win == 1 -> winning_condition(NewState, Player); Valid1 = 1).

bot_move(State, NewState, BotDif):-
    game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
    write(' > There is no possible moves'), nl,
    remove_piece(Board, NewBoard, Player, 1),
    count_pieces(NewBoard, 'R', CountCurPlayer),
    count_pieces(NewBoard, 'B', CountOpponet),    
    next_turn(Turn, NextTurn),
    game_state_pack(NewState, NewBoard, Opponent, Player, CountCurPlayer, CountOpponet, Bot1, Bot2, NextTurn).
    

% choose_move(BotDif, [HeadPieces|LPieces], [HeadMoves|LMoves], Piece, Move, Board)
% chooses from a list of moves the next move a bot should do
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
