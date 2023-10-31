:- consult(config).
:- consult(board).

% ---------- INITIAL STATE ----------
initial(Size, InitialState):-
    create_board(Size, Board),
    game_state_pack(InitialState, Board, 'Red', 'Blue').

% ---------- FINAL STATE ----------
final(State):- 
    winning_condition(State).

% ---------- MOVE ----------
move(State, NewState):-
    valid_move(State, NewState).

% ---------- PLAY ----------
play:- 
     bounce, nl,
     write(' > Size of board(4, 6 or 8): '),
     read(Size),
     initial(Size, Init),
     nl, nl,
     play(Init, [Init], States),
     reverse(States, Path), write(Path).

play(Curr, Path, Path):- 
    final(Curr), !.

play(Curr, Path, States):- 
    game_state_pack(Curr, Board, Player1, Player2),
    write(' > '), write(Player1), write(' pieces to play now!\n\n'),
    display_game(Board),
    move(Curr, Next),
    play(Next, [Next|Path], States).

game_state_pack(GameState, Board, CurrentPlayer, Opponent) :-
    GameState = [Board, CurrentPlayer, Opponent].