% AI code here

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
    %write(' > New Size: '), write(NewSize), nl,
    (
        NewSize =< OldSize ->
        update_board(State, Piece, Move, NewState, Valid),
        Valid = 0;
        (
            (Player = 'R' -> NewSize >= RedPieces; 
            Player = 'B' -> NewSize >= BluePieces),
            winning_condition(FinalState, Player);
            Valid = 2
        )
    ).


valid_move_ai(State, NewState, Move, Piece):-
        game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces),
        length(Board, Size),
        check_valid_move(State, Piece, Move, NewState, Valid),
        Valid > 1,
        !.

valid_moves(GameState, Player, ValidMoves) :-
    findall(Move, valid_move_ai(GameState, Player, Move), ValidMoves).

random_member(X, [X | _]).
random_member(X, [_ | T]) :-
    random_member(X, T).

choose_move(GameState, Player, Level, Move) :-
    valid_moves(GameState, Player, ValidMoves),
    random_member(Move, ValidMoves).

