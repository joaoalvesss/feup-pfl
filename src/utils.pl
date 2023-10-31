:- use_module(library(between)).
% Include utility predicates and helper functions here



add_list(List, Table, NewTable):-
    append( Table, [List], NewTable).



copy_tail([], TableBuild, TableBuild).
copy_tail([Head|Tail], TableBuild, NewTable):-
    add_list(Head, TableBuild, NTableBuild),
    copy_tail(Tail, NTableBuild, NewTable).





replace(Table, Row, Col, Symbol, NewTable):-
    replace(Table, Row - 1, Col - 1, 0, 0, Symbol, [], NewTable).



%Go to the correct row
replace([Head|Tail], Row, Col, Acc_row, Acc_col, Symbol, TableBuild, NewTable):-
    Row > Acc_row,
    NAcc_row is Acc_row + 1,
    add_list(Head, TableBuild, NTableBuild),
    replace(Tail, Row, Col, NAcc_row, Acc_col, Symbol, NTableBuild, NewTable),
    !.

replace([Head|Tail], _, Col, _, Acc_col, Symbol, TableBuild, NewTable):-
    replace_list_el(Head, Col, Symbol, NewList),
    add_list(NewList, TableBuild, NTableBuild),
    copy_tail(Tail, NTableBuild, NewTable).


replace_list_el( List, Index, Symbol, NewList):-
    replace_list_el( List, Index, 0, Symbol, [], NewList).



replace_list_el( [Head|Tail], Index, Acc, Symbol, [], NewList):-
    Acc < Index,
    replace_list_el(Tail, Index, Acc + 1, Symbol, [Head], NewList),
    !.



replace_list_el( [Head|Tail], Index, Acc, Symbol, ListBuild, NewList):-
    Acc < Index,
    append(ListBuild, [Head], TMP),
    replace_list_el(Tail, Index, Acc + 1, Symbol, TMP, NewList),
    !.

replace_list_el( [Head|Tail], _, _, Symbol, ListBuild, NewList):-
    append(ListBuild, [Symbol], TMP),
    append(TMP, Tail, NewList).






get_option(Min,Max,Context,Value):-
    format('~a between ~d and ~d: ', [Context, Min, Max]),
    repeat,
    read_number(Value),
    between(Min, Max, Value), !.

read_number(X):-
    read_number_aux(X,0).
read_number_aux(X,Acc):- 
    get_code(C),
    between(48, 57, C), !,
    Acc1 is 10*Acc + (C - 48),
    read_number_aux(X,Acc1).
read_number_aux(X,X).    


% ------------------------------------------------------ GET INT

get_int(N):- 
    get_int(0, N).

get_int(N, Bottom, Top):-
    get_int(0, Input),
    (between(Bottom, Top, Input) -> N = Input; get_int(N, Top, Bottom)).

get_int(N, N):- 
    peek_code(10), 
    get_code(10), 
    !.
    
get_int(Current, Result):-
    get_code(Input),
    between(48, 57, Input),
    New is Current * 10 + (Input - 48),
    get_int(New, Result).


% ------------------------------------------------------ READ INPUT

read_column(Size, Column) :-
    write(' > Column: '),
    get_int(Input),
    (Input > 0, Input =< Size -> Column = Input; read_column(Size, Column)).

read_row(Size, Row) :-
    write(' > Row: '),
    get_int(Input),
    (Input > 0, Input =< Size -> Row = Input; read_row(Size, Row)).

read_move(Size, Move) :-
    read_column(Size, Column),
    read_row(Size, Row),
    Move = (Row-Column).

read_piece(Size, Piece) :-
     read_column(Size, Column),
     read_row(Size, Row),
     Piece = (Row-Column).    


clear_console:- 
    write('\33\[2J'). 

get_element(X, Y, BOARD, ELEMENT) :-
    nth0(Y, BOARD, LINE),
    nth0(X, LINE, ELEMENT).    
