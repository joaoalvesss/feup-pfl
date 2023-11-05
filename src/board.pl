:- consult(data).
:- consult(utils).

% ---------- BOARD PRINT ----------

% print_top_indexes(+N)
% Prints the top row of column indexes for the game board.
print_top_indexes(N):-
    write('     '),
    N1 is N,
    print_top_indexes(N1, 1).

% print_top_indexes(+N, +N)
% Ends the top index printing when N is reached.    
print_top_indexes(N, N):-
    write(''),
    write(N),
    write('  '),
    nl, !.

% print_top_indexes(+N, +I)
% Continues printing top indexes up to N.    
print_top_indexes(N, I):-
    I =< N,
    I1 is I + 1,
    write(''),
    write(I),
    write('   '),
    print_top_indexes(N, I1).

% print_side_index(+N)
% Prints the side index for a specific row.
print_side_index(N):-
    write(' '),
    write(N),
    write(' ').

% print_separator_line(+N)
% Prints a separator line for the game board.
print_separator_line(N):-
    write('   '),
    print_separator_line(N, 1).

% print_separator_line(+N, +N)
% Ends separator line printing when N is reached.    
print_separator_line(N, N) :-
    write('-----'),
    nl, !.

% print_separator_line(+N, +I)
% Continues printing separator lines up to N.    
print_separator_line(N, I):-
    I =< N,
    I1 is I + 1,
    write('----'),
    print_separator_line(N, I1).

% print_line(+Line)
% Prints a row of the game board.
print_line(Line):-
    length(Line, N),
    print_line(N, 0, Line).

% print_line(+Line)
% Prints a row of the game board.
print_line(_, _, []) :- 
    write('|'),
    nl, !.

% print_line(+N, +X, +Row)
% Prints a single row of the game board.
print_line(N, X, [FirstElement | Rest]):-
    X =< N,
    X1 is X + 1,
    write('| '),
    write(FirstElement),
    write(' '),
    print_line(N, X1, Rest).

% display_game(+State)
% Displays the game board based on the provided state.
display_game(State):-
    length(State, N),
    print_top_indexes(N),
    display_game(N, 1, State).

% display_game(+N, +Y, +GameBoard)
% Calls to display the separator line after a line was all printed.
display_game(N, Y, []):-
    print_separator_line(N), nl.

% display_game(+N, +Y, +GameBoard)
% Displays the game board, including row separators and side indexes.
display_game(N, Y, [FirstLine | Rest]):-
    Y =< N,
    Y1 is Y + 1,
    print_separator_line(N),
    print_side_index(Y),
    print_line(FirstLine),
    display_game(N, Y1, Rest).


% ---------- BOARD CREATION ----------

% create_line(+0, +_, -List)
% Ends the creation of a line when N is 0.   
create_line(0, _, []):- !.

% create_line(+N, +Symbols, -NewLine)
% Creates a line of the game board with symbols.
create_line(N, [Element | Rest], [Element | NewLine]) :-
    N > 0,
    N1 is N - 1,
    create_line(N1, Rest, NewLine).

% create_board(+N, -Board)
% Creates a game board of size N with symbols.
create_board(N, Board) :-
    (   N = 4 -> board1(N, Symbols)
    ;   N = 6 -> board2(N, Symbols)
    ;   N = 8 -> board3(N, Symbols)
    ),
    create_board(N, Symbols, Board).

% create_board(+N, +[], -[])
% Ends board creation when the symbols list is empty.
create_board(_, [], []).

% create_board(+N, +[RowSymbols | Rest], -NewBoard)
% Creates a game board row by row.
create_board(N, [RowSymbols | Rest], [Row | NewBoard]) :-
    create_line(N, RowSymbols, Row),
    create_board(N, Rest, NewBoard).