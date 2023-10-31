:- consult(data).
:- consult(utils).

% Define game board and related rules here

% ---------- BOARD PRINT ----------
print_top_indexes(N):-
    write('     '),
    N1 is N,
    print_top_indexes(N1, 1).
print_top_indexes(N, N):-
    write(''),
    write(N),
    write('  '),
    nl, !.
print_top_indexes(N, I):-
    I =< N,
    I1 is I + 1,
    write(''),
    write(I),
    write('   '),
    print_top_indexes(N, I1).

print_side_index(N):-
    write(' '),
    write(N),
    write(' ').

print_separator_line(N):-
    write('   '),
    print_separator_line(N, 1).
print_separator_line(N, N) :-
    write('-----'),
    nl, !.
print_separator_line(N, I):-
    I =< N,
    I1 is I + 1,
    write('----'),
    print_separator_line(N, I1).

print_line(Line):-
    length(Line, N),
    print_line(N, 0, Line).
print_line(_, _, []) :- 
    write('|'),
    nl, !.
print_line(N, X, [FirstElement | Rest]):-
    X =< N,
    X1 is X + 1,
    write('| '),
    write(FirstElement),
    write(' '),
    print_line(N, X1, Rest).

display_game(State):-
    length(State, N),
    print_top_indexes(N),
    display_game(N, 1, State).
display_game(N, Y, []):-
    print_separator_line(N), nl.
display_game(N, Y, [FirstLine | Rest]):-
    Y =< N,
    Y1 is Y + 1,
    print_separator_line(N),
    print_side_index(Y),
    print_line(FirstLine),
    display_game(N, Y1, Rest).


% ---------- BOARD CREATION ----------
create_line(0, _, []):- !.
create_line(N, [Element | Rest], [Element | NewLine]) :-
    N > 0,
    N1 is N - 1,
    create_line(N1, Rest, NewLine).


create_board(N, Board) :-
    (   N = 4 -> board1(N, Symbols)
    ;   N = 6 -> board2(N, Symbols)
    ;   N = 8 -> board3(N, Symbols)
    ),
    create_board(N, Symbols, Board).

create_board(_, [], []).
create_board(N, [RowSymbols | Rest], [Row | NewBoard]) :-
    create_line(N, RowSymbols, Row),
    create_board(N, Rest, NewBoard).





