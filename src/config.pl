:- use_module(library(lists)).
:- consult(utils).
:- consult(data).

% ---------- MOVE VALIDATION ----------

% validate(+Received, +Expected, -Valid)
% Checks if the received and expected values match.
validate(Rcvd, Expctd, Valid) :-
            (Rcvd == Expctd -> Valid = 1; Valid = 0).

% check_valid_move(+State, +Piece, +Move, -NewState, -Valid)
% Validates a move in the "Bounce" game and updates the game state.
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
        (
            (Player = 'R' -> NewSize >= RedPieces; 
            Player = 'B' -> NewSize >= BluePieces)->
            game_over(FinalState, Player);
            Valid = 2
        )
    ).

% valid_move(+State, -NewState)
% Handles cases when there are no valid moves in the "Bounce" game.
valid_move(State, NewState):-
        game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces),
        length(Board, Size),
        \+ (possible_move(Board, Player, Size, FoundPieces, FoundMoves)),
        write(' > There is no valid move'), nl,
        remove_piece(Board, NewBoard, Player), 
        count_pieces(NewBoard, 'R', CountCurPlayer),
        count_pieces(NewBoard, 'B', CountOpponet),
        game_state_pack(NewState, NewBoard, Opponent, Player, CountCurPlayer, CountOpponet).

% valid_move(+State, -NewState)
% Reads and validates a move in the "Bounce" game, then updates the game state.
valid_move(State, NewState):-
        %write(' > There is a valid move'), nl,
        game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces),
        length(Board, Size),
        read_piece(Size, Piece), nl,
        read_move(Size, Move), nl, 
        check_valid_move(State, Piece, Move, NewState, Valid),
        Valid > 1,
        !.

% valid_move(+State, -NewState)
% Handles invalid moves and prompts for another move in the "Bounce" game.
valid_move(State, NewState):-
            write(' > Invalid move, try again!'), nl, nl, 
            valid_move(State, NewState).

% ---------- BOARD UPDATING ----------

% update_board(+State, +Move, +Piece, -NewState)
% Does nothing when Valid = 0.
update_board(State, Move, Piece, NewState, 0).

% update_board(+State, +Move, +Piece, -NewState)
% Does nothing when Valid = 1.
update_board(State, Move, Piece, NewState, 1).

% update_board(+State, +Move, +Piece, -NewState)
% Updates the game board after a valid move when Valid = 2.
update_board(State, Move, Piece, NewState, 2):- 
    Move = (Row-Column), 
    Piece = (OldRow-OldCol),
    game_state_pack(State, Board, CurrentPlayer, Opponent, RedPieces, BluePieces),
    place_piece(Row, Column, OldRow, OldCol, CurrentPlayer, Board, NewBoard),
    count_pieces(NewBoard, 'R', CountCurPlayer),
    count_pieces(NewBoard, 'B', CountOpponet),
    game_state_pack(NewState, NewBoard, Opponent, CurrentPlayer, CountCurPlayer, CountOpponet).

% place_piece(+Row, +Column, +OldRow, +OldColumn, +Element, +Board, -NewBoard)
% Moves a game piece from one position to another on the game board.
place_piece(Row, Column, OldRow, OldCol, Element, Board, NewBoard):-
        replace(Board, Row, Column, Element, TMP),
        replace(TMP, OldRow, OldCol, ' ', NewBoard).


% remove_piece(+Board, -NewBoard, +Player, -NewState)
% Removes a game piece from the game board.
remove_piece(Board, NewBoard, Player, NewState):-
        length(Board, Size),
        read_piece(Size, Piece),
        Piece = (Row-Column),
        get_el(Board, Row, Column, Element),
        (
            Element == Player -> replace(Board, Row, Column, ' ', NewBoard);
            write('Invalid piece to remove'), remove_piece(Board, NewBoard, Player)
        ).

% ---------- WINNING CONDITION ----------

% game_over(-GameState, +Winner)
% Handles the game over condition, displays the winner, and offers the choice to play again or exit.
game_over(GameState, Winner):-
    clear_console, 
    nl, nl,
    (
        Winner = 'R' -> red_wins;
        Winner = 'B' -> blue_wins
    ), 
    nl, write(' > Enter "play." to start a new game or "exit." to quit: '), nl, 
    read(PlayerChoice),
    write(PlayerChoice),
    handle_user_choice(PlayerChoice).

% handle_user_choice(+Choice)
% Handles the players choice after the game is over, allows playing again.
handle_user_choice('play'):- 
    play.

% handle_user_choice(+Choice)
% Handles the players choice after the game is over, allows exiting the game.
handle_user_choice('exit'):- 
    write(' > Thanks for playing! Goodbye.'), halt.

% handle_user_choice(+Choice)
% Handles an invalid choice and prompts for a new one.
handle_user_choice(Other) :- 
    nl, write(' > Invalid choice. Enter "play." to start a new game or "exit." to quit: '),
    read(NewChoice),
    handle_user_choice(NewChoice).

