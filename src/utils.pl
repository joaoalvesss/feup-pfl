:- use_module(library(between)).

% ---------- INSERTION & REPLACING ----------

% adds a list to a table
add_list(List, Table, NewTable):-
    append( Table, [List], NewTable).


% copies the tail of a list to a table 
copy_tail([], TableBuild, TableBuild).
copy_tail([Head|Tail], TableBuild, NewTable):-
    add_list(Head, TableBuild, NTableBuild),
    copy_tail(Tail, NTableBuild, NewTable).


% replaces the element stored on the -Table at the given -Row and -Col by -Symbol
replace(Table, Row, Col, Symbol, NewTable):-
    NRow is Row - 1,
    NCol is Col - 1,
    replace(Table, NRow, NCol, 0, 0, Symbol, [], NewTable).

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


% replaces the element stored on the -Listat the given -Index by -Symbol
replace_list_el( List, Index, Symbol, NewList):-
    replace_list_el( List, Index, 0, Symbol, [], NewList).

replace_list_el( [Head|Tail], Index, Acc, Symbol, [], NewList):-
    Acc < Index,
    NAcc is Acc + 1,
    replace_list_el(Tail, Index, NAcc, Symbol, [Head], NewList),
    !.

replace_list_el( [Head|Tail], Index, Acc, Symbol, ListBuild, NewList):-
    Acc < Index,
    append(ListBuild, [Head], TMP),
    NAcc is Acc + 1,
    replace_list_el(Tail, Index, NAcc, Symbol, TMP, NewList),
    !.

replace_list_el( [Head|Tail], _, _, Symbol, ListBuild, NewList):-
    append(ListBuild, [Symbol], TMP),
    append(TMP, Tail, NewList).

% ---------- SEARCH ----------

% stores on -Element the value stored at -Index of -List
get_el_list(List, Index, Element):-
    get_el_list(List, Index - 1, 0, Element).

get_el_list([Head|Tail], Index, Acc, Element):-
    Acc < Index,
    get_el_list(Tail, Index, Acc + 1, Element),
    !.

get_el_list([Head|Tail], Index, _, Element):-
    Element = Head.


% stores on -Element the value stored at position (-Row, -Col) on -Board
get_el(Board, Row, Col, Element):-
    get_el_board(Board, Row - 1, Col, 0, Element).
    
get_el_board([Head|Tail], Row, Col, Acc, Element):-
    Acc < Row,
    get_el_board(Tail, Row, Col, Acc + 1, Element),
    !.

get_el_board([Head|Tail], Row, Col, _, Element):-
    get_el_list(Head, Col, Element).


% finds and stores on -Piece the position on -Board of the first element with value -Player
find_piece(Board, BoardSize, Player, Piece):-
    Pos = (1-1),
    find_piece(Board, BoardSize, Player, Piece, Pos).

find_piece(Board, BoardSize, Player, Piece, Pos):-
    Pos = (Row-Col),
    get_el(Board, Row, Col, Element),
    \+ (Element == Player),
    \+ (last_move(Pos, BoardSize)),
    next_move(Pos, NPos, BoardSize),
    find_piece(Board, BoardSize, Player, Piece, NPos),
    !.

find_piece(Board, BoardSize, Player, Piece, Pos):-
    Pos = (Row-Col),
    get_el(Board, Row, Col, Element),
    Element == Player,
    Piece = Pos.


% ---------- CHECK ---------------

% evaluates and checks if a move is possible
move_eval(Board, Piece, Move, Player, BoardSize):-
    Move = (Row-Column),
    get_el(Board, Row, Column, Element1),
    Element1 == ' ',
    !,
    Piece = (OldRow-OldColumn),
    get_el(Board, OldRow, OldColumn, Element2),
    Element2 == Player,
    !,
    bfs([Piece], OldSize, Player, Board), !, 
    place_piece(Row, Column, OldRow, OldColumn, Player, Board, NewBoard),
    bfs([Move], NewSize, Player, NewBoard), 
    NewSize > OldSize,
    !.


% possible_move(Board, Player, BoardSize, ListPieces, ListMoves)
% finds and stores all the possible moves on -ListPieces and -ListMoves
% -ListPieces lists where a move starts and respective index of -ListMoves
% lists the valid move associated to that piece
possible_move(Board, Player, BoardSize, FoundP, FoundM):-
    Move = (1-1),
    possible_move(Board, Move, Move, Player, BoardSize, FoundP, FoundM, [], []).

possible_move(Board, Piece, Move, Player, BoardSize, FoundP, FoundM, Acc1, Acc2):-
    last_move_combination(Piece, Move, BoardSize), 
    move_eval(TmpBoard, Piece, Move, Player, BoardSize),
    append(Acc1, [Piece], NAcc1),
    append(Acc2, [Move], NAcc2),
    FoundP = Acc1,
    FoundM = Acc2,
    !.

possible_move(Board, Piece, Move, Player, BoardSize, FoundP, FoundM, Acc1, Acc2):-
    last_move_combination(Piece, Move, BoardSize), 
    FoundP = Acc1,
    FoundM = Acc2,
    !.

possible_move(Board, Piece, Move, Player, BoardSize, FoundP, FoundM, Acc1, Acc2):-
    \+ (last_move_combination(Piece, Move, BoardSize)),
    TmpBoard = Board,    
    \+ (move_eval(TmpBoard, Piece, Move, Player, BoardSize)), 
    next_move_combination(Piece, Move, NPiece, NMove, BoardSize),
    possible_move(Board, NPiece, NMove, Player, BoardSize, FoundP, FoundM, Acc1, Acc2),
    !.

possible_move(Board, Piece, Move, Player, BoardSize, FoundP, FoundM, Acc1, Acc2):-
    TmpBoard = Board,
    move_eval(TmpBoard, Piece, Move, Player, BoardSize),
    next_move_combination(Piece, Move, NPiece, NMove, BoardSize),
    append(Acc1, [Piece], NAcc1),
    append(Acc2, [Move], NAcc2),
    possible_move(Board, NPiece, NMove, Player, BoardSize, FoundP, FoundM, NAcc1, NAcc2),
    !.


% remove_piece(Board, NewBoard, Player, UseBot)
% removes a piece of the board, if -UseBot is set to 1, user input is ignored, 
% and the bot will choose a move
remove_piece(Board, NewBoard, Player, 0):-
        length(Board, Size),
        write(' > Choose a piece to remove : '), nl,
        read_piece(Size, Piece),
        Piece = (Row-Column),
        get_el(Board, Row, Column, Element),
        (
            Element == Player -> replace(Board, Row, Column, ' ', NewBoard);
            write('Invalid piece to remove'), remove_piece(Board, NewBoard, Player, 0)
        ).

remove_piece(Board, NewBoard, Player, 1):-
        length(Board, Size),
        find_piece(Board, Size, Player, Piece),
        Piece = (Row-Column),
        replace(Board, Row, Column, ' ', NewBoard).


% ---------- GET NUMBER ----------

% converts ASCII to the respective number
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


% --------- NEXT MOVE ---------------

% Checks if -Move is equal to the last position on a board with size equal
% to (-BoardSize x -BoardSize)
last_move(Move, BoardSize):-
    Move = (Row-Col),
    Row == BoardSize,
    Col == BoardSize.


% Checks if the givn move combination (-Piece, -Move) is the last one according to
% the specification on last_move
last_move_combination(Piece, Move, BoardSize):-
    last_move(Move, BoardSize),
    last_move(Piece, BoardSize).


% Stores on -Next_Move the next move after -Move
next_move(Move, Next_Move, BoardSize):-
    Move = (Row-Col),
    Row =< BoardSize,
    Col < BoardSize,
    NCol is Col + 1,
    Next_Move = (Row-NCol),
    !.

next_move(Move, Next_Move, BoardSize):-
    Move = (Row-Col),
    Row < BoardSize,
    Col == BoardSize,
    NRow is Row + 1,
    Next_Move = (NRow-1),
    !.

% stores on -NPiece and -NMove the next respective move 
next_move_combination(Piece, Move, NPiece, NMove, BoardSize):-
    Piece = (OldRow-OldCol),
    OldRow == BoardSize,
    OldCol == BoardSize,
    NPiece = Piece,
    Move = (NewRow-NewCol),
    NewRow =< BoardSize,
    NewCol < BoardSize,
    next_move(Move, NMove, BoardSize),
    !.

next_move_combination(Piece, Move, NPiece, NMove, BoardSize):-
    Piece = (OldRow-OldCol),
    OldRow < BoardSize,
    Move = (NewRow-NewCol),
    NewRow == BoardSize,
    NewCol == BoardSize,
    NMove = (1-1),
    next_move(Piece, NPiece, BoardSize),
    !.

next_move_combination(Piece, Move, NPiece, NMove, BoardSize):-
    Piece = (OldRow-OldCol),
    OldRow == BoardSize,
    OldCol < BoardSize,
    Move = (NewRow-NewCol),
    NewRow == BoardSize,
    NewCol == BoardSize,
    NMove = (1-1),
    next_move(Piece, NPiece, BoardSize),
    !.
    
next_move_combination(Piece, Move, NPiece, NMove, BoardSize):-  
    NPiece = Piece,
    next_move(Move, NMove, BoardSize),
    !.

% ---------- INPUTS -----------

% reads input for -Column
read_column(Size, Column) :-
    write(' > Column: '),
    get_int(Input),
    (Input > 0, Input =< Size -> Column = Input; read_column(Size, Column)).


% reads input for -Row
read_row(Size, Row) :-
    write(' > Row: '),
    get_int(Input),
    (Input > 0, Input =< Size -> Row = Input; read_row(Size, Row)).


% reads input for -Move
read_move(Size, Move) :-
    read_column(Size, Column),
    read_row(Size, Row),
    Move = (Row-Column).


% reads input for -Piece
read_piece(Size, Piece) :-
     read_column(Size, Column),
     read_row(Size, Row),
     Piece = (Row-Column).    


% ---------- OTHERS ----------

% clears console
clear_console:- 
    write('\33\[2J').  

% --------- BFS -------------

% finds the size of a list
list_size(List, Size):-
    list_size(List, Size, 0).

list_size([], Size, Acc):-
    Size is Acc. 

list_size([Head|Tail], Size, Acc):-
    NAcc is Acc + 1,
    list_size(Tail, Size, NAcc). 


% bfs(List, GroupSize, Player, Board)
% performs a bfs search
% sets -GroupSize with the size of the group where the bfs started
% the bfs starts on the head of the list -List and counts how many elements 
% equal to player there are in the respective group
bfs(List, GroupSize, Player, Board):-
    bfs(List, GroupSize, 0, Player, Board, [], [], Visited).

bfs([], GroupSize, Acc, Player, Board, Visited, MyVisited, FinalVisited):-
    sort(MyVisited, FinalVisited),
    list_size(FinalVisited, GroupSize).

bfs([Head|Tail], GroupSize, Acc, Player, Board, Visited, MyVisited, FinalVisited):-
    Head = (Row-Col),
    append(Visited, [Head], NVisited),                                                       
    get_el(Board, Row, Col, Element),                           
    (
    Element == Player ->
        NAcc is Acc + 1,
        neighbor_positions(Board, Row, Col, NeighborList, Visited), 
        append(Tail, NeighborList, NTail),
        append(MyVisited, [Head], NMyVisited),
        bfs(NTail, GroupSize, NAcc, Player, Board, NVisited, NMyVisited, FinalVisited);
        bfs(Tail, GroupSize, Acc, Player, Board, NVisited, MyVisited, FinalVisited)
    ).


% checks adjacent elements 
neighbor_positions(Board, Row, Col, NeighborList, Visited):-
    neighbor_offsets(Offsets),
    findall((NewRow-NewCol), (
        member((OffsetRow-OffsetCol), Offsets),
        NewRow is Row + OffsetRow,
        NewCol is Col + OffsetCol,
        valid_position(Board, NewRow, NewCol, Visited)
    ), NeighborList).


neighbor_offsets([((1)-(0)), ((-1)-(0)), ((0)-(1)), ((0)-(-1))]).  % Up, Down, Left, Right


% Checks if a position is valid
valid_position(Board, Row, Col, Visited):-
    length(Board, NumRows),
    length(Board, NumCols),
    Row >= 1, Row =< NumRows,
    Col >= 1, Col =< NumCols,
    \+ (member((Row-Col), Visited)).
 

% counts how many pieces of color -Color there are on the board
count_pieces(Board, Color, Count) :-
    count_pieces(Board, Color, 0, Count).

count_pieces([], _, Count, Count).

count_pieces([Row|Rest], Color, CurrentCount, Count) :-
    count_row(Row, Color, RowCount),
    NewCount is CurrentCount + RowCount,
    count_pieces(Rest, Color, NewCount, Count).

count_row([], _, 0).
count_row([Piece|Rest], Color, Count) :-
    (Piece = Color -> RowCount = 1; RowCount = 0),
    count_row(Rest, Color, RestCount),
    Count is RowCount + RestCount.


% ---------- ASCII ARTS ----------

red_wins:-
    write(' ____          _  __        ___           _ '), nl,
    write('|  _ \\ ___  __| | \\ \\      / (_)_ __  ___| |'), nl,
    write('| |_) / _ \\/ _` |  \\ \\ / /| | \'_ \\/ __| |'), nl,
    write('|  _ <  __/ (_| |   \\ V  V / | | | | \\__ |_|'), nl,
    write('|_| \\_\\___|\\__,_|    \\_/\\_/  |_|_| |_|___(_)'), nl.


blue_wins:-
    write(' ____  _             __        ___           _ '), nl,
    write('| __ )| |_   _  ___  \\ \\      / (_)_ __  ___| |'), nl,
    write('|  _ \\| | | | |/ _ \\  \\ \\ / /| | \'_ \\/ __| |'), nl,
    write('| |_) | | |_| |  __/   \\ V  V / | | | | \\__ |_|'), nl,
    write('|____/|_|\\__,_|\\___|    \\_/\\_/  |_|_| |_|___(_)'), nl.


bounce:-
     write('  ____                              \n'),                          
     write(' | __ )  ___  _   _ _ __   ___ ___  \n'),
     write(' |  _ \\ / _ \\| | | | |_ \\ / __/ _ \\ \n'),
     write(' | |_) | (_) | |_| | | | | (_|  __/ \n'),
     write(' |____/ \\___/ \\__,_|_| |_|\\___\\___| \n'),
     write('\n'),
     write(' ---------------------------------\n'),
     write(' |            Welcome            |\n'),
     write(' |         To Game Menu!         |\n'),
     write(' ---------------------------------\n').


bounce_game:-
     write('  ____                              \n'),                          
     write(' | __ )  ___  _   _ _ __   ___ ___  \n'),
     write(' |  _ \\ / _ \\| | | | |_ \\ / __/ _ \\ \n'),
     write(' | |_) | (_) | |_| | | | | (_|  __/ \n'),
     write(' |____/ \\___/ \\__,_|_| |_|\\___\\___| \n').



% ----------------- TURN ------------------

% sets next turn value
next_turn(1, 2).
next_turn(2,1).


% true when receives 2 empty lists
empty_list([], []).


