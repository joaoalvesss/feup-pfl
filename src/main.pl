:- consult(config).
:- consult(board).

% ---------- INITIAL STATE ----------

% initial(+Size, -InitialState)
% Initializes a new game state with the given board size.
initial(Size, InitialState) :-
    create_board(Size, Board),
    count_pieces(Board, 'R', RedPieces),
    count_pieces(Board, 'B', BluePieces),
    game_state_pack(InitialState, Board, 'R', 'B', RedPieces, BluePieces).

% ---------- MOVE ----------

% move(+State, -NewState)
% Manages a players move and updates the game state.
move(State, NewState):-
        valid_move(State, NewState).

% ---------- PLAY ----------

% play
% Initiates and oversees the gameplay of the "Bounce" game.
play:- 
        clear_console,
        bounce, nl,
        write(' > Size of board("4.", "6." or "8."): '),
        read(Size),
        % 'ADICIONAR PEDIDO DE HxH, HxP, PxP'
        % 'SE P, PERGUNTAR DIFICULDADE'
        initial(Size, Init),
        nl, nl,
        play(Init, [Init], States),
        reverse(States, Path), write(Path).


% play(+Curr, +Path, -States)
% Manages the turn-based gameplay of the "Bounce" game.
play(Curr, Path, States):-
        game_state_pack(Curr, Board, Player1, Player2, RedPieces, BluePieces),
        clear_console, nl, nl, bounce_game, nl,
        (
                Player1 == 'R' ->
                write(' > Red checkers to play now!'), nl, nl;
                write(' > Blue checkers to play now!'), nl, nl
        ),
        display_game(Board),
        % 'ADICIONAR QUEM É A JOGAR'
        move(Curr, Next),
        play(Next, [Next|Path], States).

% game_state_pack(-GameState, +Board, +CurrentPlayer, +Opponent, +RedPieces, +BluePieces)
% Packs the game state with board, player information, and piece counts.
game_state_pack(GameState, Board, CurrentPlayer, Opponent, RedPieces, BluePieces) :-
        GameState = [Board, CurrentPlayer, Opponent, RedPieces, BluePieces].
