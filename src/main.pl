:- consult(config).
:- consult(board).

% ---------- INITIAL STATE ----------
initial(Size, InitialState):-
    create_board(Size, Board),
    game_state_pack(InitialState, Board, '1', '2').

% ---------- FINAL STATE ----------
final(State):- 
    winning_condition(State).

% ---------- MOVE ----------
move(State, NewState):-
    valid_move(State, NewState).

% ---------- PLAY ----------
play:- 
     bounce, 
     write('Size of board(4, 6 or 8): '),
     read(Size),
     initial(Size, Init), % Ainda esta hardcoded
     nl, nl, nl,
     play(Init, [Init], States),
     reverse(States, Path), write(Path).

play(Curr, Path, Path):- 
    final(Curr), !.

play(Curr, Path, States):- 
    game_state_pack(Curr, Board, Player1, Player2),
    display_game(Board),
    move(Curr, Next),
    play(Next, [Next|Path], States).

game_state_pack(GameState, Board, CurrentPlayer, Opponent) :-
    GameState = [Board, CurrentPlayer, Opponent].