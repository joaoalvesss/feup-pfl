:- consult(config).
:- consult(board).
:- consult(bot).

% ---------- INITIAL STATE ----------

% initial(Option, Bot1, Bot2,Size, InitialState)
% prepares game elements according to the user specification 
initial_state(1, Bot1, Bot2, Size, InitialState) :-
    create_board(Size, Board),
    count_pieces(Board, 'R', RedPieces),
    count_pieces(Board, 'B', BluePieces),
    game_state_pack(InitialState, Board, 'R', 'B', RedPieces, BluePieces, Bot1, Bot2, 1).

initial_state(2, Bot1, Bot2, Size, InitialState) :-
    create_board(Size, Board),
    count_pieces(Board, 'R', RedPieces),
    count_pieces(Board, 'B', BluePieces),
    game_state_pack(InitialState, Board, 'R', 'B', RedPieces, BluePieces, Bot1, Bot2, 1).


initial_state(3, Bot1, Bot2, Size, InitialState) :-
    create_board(Size, Board),
    count_pieces(Board, 'R', RedPieces),
    count_pieces(Board, 'B', BluePieces),
    game_state_pack(InitialState, Board, 'R', 'B', RedPieces, BluePieces, Bot1, Bot2, 1).


% ---------- MOVE ----------

% move(State, NewState)
% prepares and does the next move 
move(State, NewState):-
        game_state_pack(State, Board, Player, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn),
        move(State, NewState, Turn, Bot1, Bot2).

move(State, NewState, 1, Bot1, Bot2):-
        Bot1 == 0,
        write(' > Turn 1, the player plays!'), nl,
        move_user(State, NewState),
        write(' > Turn 1, the player played!'), nl, nl,
        !.

move(State, NewState, 1, Bot1, Bot2):-
        write(' > Turn 1, the computer plays!'), nl,
        bot_move(State, NewState, Bot1),
        write(' > Turn 1, the computer played!'), nl, nl.

move(State, NewState, 2, Bot1, Bot2):-
        Bot2 == 0,
        write(' > Turn 2, the player plays!'), nl,
        move_user(State, NewState),
        write(' > Turn 2, the player played!'), nl, nl,
        !.

move(State, NewState, 2, Bot1, Bot2):-
        write(' > Turn 2, the computer plays!'), nl,
        bot_move(State, NewState, Bot2),
        write(' > Turn 2, the computer played!'), nl, nl.

% ---------- PLAY ----------

% starts the game
play:- 
        clear_console,
        bounce, nl,
        write(' > Size of board("4.", "6." or "8."): '),
        read(Size),
        nl,
        write(' > "1." Human vs Human / "2." Human vs Bot / "3." Bot vs Bot: '),
        read(Opt),
        nl,
        config_bot(Opt, Bot1, Bot2),
        initial_state(Opt, Bot1, Bot2, Size, Init),
        nl, nl,
        play(Init, [Init], States),
        reverse(States, Path), write(Path).


% play(Curr, Path, States)
% Manages the game flow
play(Curr, Path, States):-
        game_state_pack(Curr, Board, Player1, Player2, RedPieces, BluePieces, Bot1, Bot2, Turn),
        (
                Player1 == 'R' ->
                write(' > Red checkers to play now!'), nl, nl;
                write(' > Blue checkers to play now!'), nl, nl
        ),
        display_game(Board),
        move(Curr, Next),
        play(Next, [Next|Path], States).


% Represents the game elements 
game_state_pack(GameState, Board, CurrentPlayer, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn) :-
        GameState = [Board, CurrentPlayer, Opponent, RedPieces, BluePieces, Bot1, Bot2, Turn].
