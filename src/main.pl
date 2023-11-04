:- consult(config).
:- consult(board).

% ---------- INITIAL STATE ----------
initial(Size, InitialState) :-
    create_board(Size, Board),
    count_pieces(Board, 'R', RedPieces),
    count_pieces(Board, 'B', BluePieces),
    game_state_pack(InitialState, Board, 'R', 'B', RedPieces, BluePieces).

% ---------- MOVE ----------
move(State, NewState):-
        valid_move(State, NewState).

% ---------- PLAY ----------
play:- 
        clear_console,
        bounce, nl,
        write(' > Size of board(4, 6 or 8): '),
        read(Size),
        initial(Size, Init),
        nl, nl,
        play(Init, [Init], States),
        reverse(States, Path), write(Path).

play(Curr, Path, States):-
        game_state_pack(Curr, Board, Player1, Player2, RedPieces, BluePieces),
        %clear_console, nl, nl, bounce_game, nl,
        (
                Player1 == 'R' ->
                write(' > Red checkers to play now!'), nl, nl;
                write(' > Blue checkers to play now!'), nl, nl
        ),
        write(' > Red Pieces: '), write(RedPieces), nl,
        write(' > Blue Pieces: '), write(BluePieces), nl, nl, 
        display_game(Board),
        move(Curr, Next),
        play(Next, [Next|Path], States).

game_state_pack(GameState, Board, CurrentPlayer, Opponent, RedPieces, BluePieces) :-
        GameState = [Board, CurrentPlayer, Opponent, RedPieces, BluePieces].
