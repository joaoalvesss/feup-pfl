:- consult(config).
:- consult(board).
:- consult(bot).


% ---------- INITIAL STATE ----------

initial(1, Bot1, Bot2, Size, InitialState) :-
    create_board(Size, Board),
    count_pieces(Board, 'R', RedPieces),
    count_pieces(Board, 'B', BluePieces),
    game_state_pack(InitialState, Board, 'R', 'B', RedPieces, BluePieces, Bot1, Bot2, 1).

initial(2, Bot1, Bot2, Size, InitialState) :-
    create_board(Size, Board),
    count_pieces(Board, 'R', RedPieces),
    count_pieces(Board, 'B', BluePieces),
    game_state_pack(InitialState, Board, 'R', 'B', RedPieces, BluePieces, Bot1, Bot2, 1).


initial(3, Bot1, Bot2, Size, InitialState) :-
    create_board(Size, Board),
    count_pieces(Board, 'R', RedPieces),
    count_pieces(Board, 'B', BluePieces),
    game_state_pack(InitialState, Board, 'R', 'B', RedPieces, BluePieces, Bot1, Bot2, 1).

initial(Size, InitialState) :-
    create_board(Size, Board),
    count_pieces(Board, 'R', RedPieces),
    count_pieces(Board, 'B', BluePieces),
    game_state_pack(InitialState, Board, 'R', 'B', RedPieces, BluePieces, 1).

% ---------- MOVE ----------

move(State, NewState):-
        game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
        move(State, NewState, Turn, Bot1, Bot2).


move(State, NewState, 1, Bot1, Bot2):-
        Bot1 == 0,
        write('Turn 1, player plays'), nl,
        valid_move(State, NewState),
        write('Turn 1, player played'), nl,
        !.

move(State, NewState, 1, Bot1, Bot2):-
        write('Turn 1, bot plays'), nl,
        bot_move(State, NewState, Bot1),
        write('Turn 1, bot played'), nl
        .



move(State, NewState, 2, Bot1, Bot2):-
        Bot2 == 0,
        write('Turn 2, player plays'), nl,
        valid_move(State, NewState),
        write('Turn 2, player played'), nl,
        !.

move(State, NewState, 2, Bot1, Bot2):-
        write('Turn 2, bot plays'), nl,
        bot_move(State, NewState, Bot2),
        write('Turn 2, bot played'), nl
        .



% ---------- PLAY ----------
play:- 
        clear_console,
        bounce, nl,
        write(' > Size of board("4.", "6." or "8."): '),
        read(Size),
        write(' > "1." Human vs Human / "2." Human vs Bot / "3." Bot vs Bot : '), nl,
        read(Opt),
        config_bot(Opt, Bot1, Bot2),
        initial(Opt, Bot1, Bot2, Size, Init),
        nl, nl,
        play(Init, [Init], States),
        reverse(States, Path), write(Path).

play(Curr, Path, States):-
        game_state_pack(Curr, Board, Player1, Player2, RedPieces, BluePieces, Bot1, Bot2, Turn),
        %clear_console, nl, nl, bounce_game, nl,
        (
                Player1 == 'R' ->
                write(' > Red checkers to play now!'), nl, nl;
                write(' > Blue checkers to play now!'), nl, nl
        ),
        %write(' > Red Pieces: '), write(RedPieces), nl,
        %write(' > Blue Pieces: '), write(BluePieces), nl, nl, 
        display_game(Board),
        move(Curr, Next),
        play(Next, [Next|Path], States).

game_state_pack(GameState, Board, CurrentPlayer, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn) :-
        GameState = [Board, CurrentPlayer, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn].
