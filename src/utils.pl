:- use_module(library(between)).

% ---------- INSERTION & REPLACING ----------

% add_list(+List, +Table, -NewTable)
% Appends a list to the end of a table, creating a new table.
add_list(List, Table, NewTable):-
    append( Table, [List], NewTable).

% copy_tail(+Source, +TableBuild, -NewTable)
% Copies the tail of a list of lists to a table.
copy_tail([], TableBuild, TableBuild).
copy_tail([Head|Tail], TableBuild, NewTable):-
    add_list(Head, TableBuild, NTableBuild),
    copy_tail(Tail, NTableBuild, NewTable).

% replace(+Table, +Row, +Col, +Symbol, -NewTable)
% Replaces an element in the table at the specified row and column with a new symbol.
replace(Table, Row, Col, Symbol, NewTable):-
    NRow is Row - 1,
    NCol is Col - 1,
    replace(Table, NRow, NCol, 0, 0, Symbol, [], NewTable).

% replace(+Table, +Row, +Col, +Symbol, -NewTable)
% Replaces an element in the table at the specified row and column with a new symbol.
replace([Head|Tail], Row, Col, Acc_row, Acc_col, Symbol, TableBuild, NewTable):-
    Row > Acc_row,
    NAcc_row is Acc_row + 1,
    add_list(Head, TableBuild, NTableBuild),
    replace(Tail, Row, Col, NAcc_row, Acc_col, Symbol, NTableBuild, NewTable),
    !.

% replace(+Table, +Row, +Col, +Symbol, -NewTable)
% Replaces an element in the table at the specified row and column with a new symbol.
replace([Head|Tail], _, Col, _, Acc_col, Symbol, TableBuild, NewTable):-
    replace_list_el(Head, Col, Symbol, NewList),
    add_list(NewList, TableBuild, NTableBuild),
    copy_tail(Tail, NTableBuild, NewTable).

% replace_list_el(+List, +Index, +Symbol, -NewList)
% Replaces an element in a list at the specified index with a new symbol, creating a new list.
replace_list_el( List, Index, Symbol, NewList):-
    replace_list_el( List, Index, 0, Symbol, [], NewList).

% replace_list_el(+List, +Index, +Symbol, -NewList)
% Replaces an element in a list at the specified index with a new symbol, creating a new list.
replace_list_el( [Head|Tail], Index, Acc, Symbol, [], NewList):-
    Acc < Index,
    NAcc is Acc + 1,
    replace_list_el(Tail, Index, NAcc, Symbol, [Head], NewList),
    !.

% replace_list_el(+List, +Index, +Symbol, -NewList)
% Replaces an element in a list at the specified index with a new symbol, creating a new list.
replace_list_el( [Head|Tail], Index, Acc, Symbol, ListBuild, NewList):-
    Acc < Index,
    append(ListBuild, [Head], TMP),
    NAcc is Acc + 1,
    replace_list_el(Tail, Index, NAcc, Symbol, TMP, NewList),
    !.

% replace_list_el(+List, +Index, +Symbol, -NewList)
% Replaces an element in a list at the specified index with a new symbol, creating a new list.
replace_list_el( [Head|Tail], _, _, Symbol, ListBuild, NewList):-
    append(ListBuild, [Symbol], TMP),
    append(TMP, Tail, NewList).

% ---------- SEARCH ----------

% get_el_list(+List, +Index, -Element)
% Retrieves the element at a specified index in a list.
get_el_list(List, Index, Element):-
    get_el_list(List, Index - 1, 0, Element).

% get_el_list(+List, +Index, +Acc, -Element)
% Helper predicate for get_el_list/3. Traverses elements in a list.
get_el_list([Head|Tail], Index, Acc, Element):-
    Acc < Index,
    get_el_list(Tail, Index, Acc + 1, Element),
    !.

% get_el_list(+List, +Index, +Acc, -Element)
% Helper predicate for get_el_list/3. Traverses elements in a list.
get_el_list([Head|Tail], Index, _, Element):-
    Element = Head.

% get_el(+Board, +Row, +Col, -Element)
% Retrieves the element at the specified row and column in a 2D board.
get_el(Board, Row, Col, Element):-
    get_el_board(Board, Row - 1, Col, 0, Element).

% get_el_board(+Board, +Row, +Col, +Acc, -Element)
% Helper predicate for get_el/4. Traverses rows of the 2D board.    
get_el_board([Head|Tail], Row, Col, Acc, Element):-
    Acc < Row,
    get_el_board(Tail, Row, Col, Acc + 1, Element),
    !.

% get_el_board(+Board, +Row, +Col, +Acc, -Element)
% Helper predicate for get_el/4. Traverses rows of the 2D board.
get_el_board([Head|Tail], Row, Col, _, Element):-
    get_el_list(Head, Col, Element).

% ---------- CHECK ---------------

% move_eval(+Board, +Piece, +Move, +Player, +BoardSize)
% Evaluates the validity of a move on the game board.
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

% possible_move(+Board, +Player, +BoardSize, -FoundPieces, -FoundMoves)
% Generates a list of possible moves for the current player.
% This predicate iteratively explores all possible combinations of moves, checking for their validity.
% It returns the lists of pieces and moves for the current players possible valid moves.
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


% ---------- GET NUMBER ----------

% get_int(-N)
% Reads an integer from the standard input.
get_int(N):- 
    get_int(0, N).

% get_int(-N, +Bottom, +Top)
% Reads an integer within a specified range from the standard input.
get_int(N, Bottom, Top):-
    get_int(0, Input),
    (between(Bottom, Top, Input) -> N = Input; get_int(N, Top, Bottom)).

% get_int(+Current, -Result)
% Helper predicate for get_int/3 and get_int/2.
get_int(N, N):- 
    peek_code(10), 
    get_code(10), 
    !.

% get_int(+N, +N)
% Helper predicate for get_int/1, marking the end of integer input.    
get_int(Current, Result):-
    get_code(Input),
    between(48, 57, Input),
    New is Current * 10 + (Input - 48),
    get_int(New, Result).


% --------- NEXT MOVE ---------------

% last_move(+Move, +BoardSize)
% Checks if the given move represents the last possible move on the game board.
last_move(Move, BoardSize):-
    Move = (Row-Col),
    Row == BoardSize,
    Col == BoardSize.

% last_move_combination(+Piece, +Move, +BoardSize)
% Checks if the provided piece and move combination represents the last possible combination of moves.
last_move_combination(Piece, Move, BoardSize):-
    last_move(Move, BoardSize),
    last_move(Piece, BoardSize).

% next_move(+Move, -NextMove, +BoardSize)
% Generates the next move based on the provided move.
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


% next_move_combination(+Piece, +Move, -NextPiece, -NextMove, +BoardSize)
% Generates the next combination of piece and move based on the provided piece and move.
% This predicate generates the next combination of piece and move positions based on the current piece and move
% to explore all possible move combinations on the game board.
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

% read_column(+Size, -Column)
% Reads and validates a column input from the user.
read_column(Size, Column) :-
    write(' > Column: '),
    get_int(Input),
    (Input > 0, Input =< Size -> Column = Input; read_column(Size, Column)).

% read_row(+Size, -Row)
% Reads and validates a row input from the user.
read_row(Size, Row) :-
    write(' > Row: '),
    get_int(Input),
    (Input > 0, Input =< Size -> Row = Input; read_row(Size, Row)).

% read_move(+Size, -Move)
% Reads a move input from the user, including both row and column.
read_move(Size, Move) :-
    read_column(Size, Column),
    read_row(Size, Row),
    Move = (Row-Column).

% read_piece(+Size, -Piece)
% Reads a piece input from the user, including both row and column.
read_piece(Size, Piece) :-
     read_column(Size, Column),
     read_row(Size, Row),
     Piece = (Row-Column).    

% ---------- OTHERS ----------

% clear_console
% Clears the console screen.
clear_console:- 
    write('\33\[2J'). 

% get_element(+X, +Y, +BOARD, -ELEMENT)
% Retrieves an element from the game board at the specified coordinates.
get_element(X, Y, BOARD, ELEMENT) :-
    nth0(Y, BOARD, LINE),
    nth0(X, LINE, ELEMENT).    

% --------- BFS -------------

% bfs(+List, -GroupSize, +Player, +Board)
% Computes the size of a connected group of pieces on the game board.
% This predicate uses a breadth-first search (BFS) algorithm to calculate the size
% of a connected group of pieces on the game board, belonging to the given player.
bfs(List, GroupSize, Player, Board):-
    bfs(List, GroupSize, 0, Player, Board, []).

bfs([], GroupSize, Acc, Player, Board, Visited):-
    GroupSize is Acc.

bfs([Head|Tail], GroupSize, Acc, Player, Board, Visited):-
    Head = (Row-Col),
    append(Visited, [Head], NVisited),                                                        
    get_el(Board, Row, Col, Element),                           
    (
    Element == Player ->
        NAcc is Acc + 1,
        neighbor_positions(Board, Row, Col, NeighborList, Visited),
        append(Tail, NeighborList, NTail),
        bfs(NTail, GroupSize, NAcc, Player, Board, NVisited);
        bfs(Tail, GroupSize, Acc, Player, Board, NVisited)
    ).

% neighbor_positions(+Board, +Row, +Col, -NeighborList, +Visited)
% Finds neighboring positions to a given position on the game board.
neighbor_positions(Board, Row, Col, NeighborList, Visited):-
    neighbor_offsets(Offsets),
    findall((NewRow-NewCol), (
        member((OffsetRow-OffsetCol), Offsets),
        NewRow is Row + OffsetRow,
        NewCol is Col + OffsetCol,
        valid_position(Board, NewRow, NewCol, Visited)
    ), NeighborList).

% neighbor_offsets(-Offsets)
% Defines offsets to navigate to neighboring positions (Up, Down, Left, Right).
neighbor_offsets([((1)-(0)), ((-1)-(0)), ((0)-(1)), ((0)-(-1))]).  % Up, Down, Left, Right

% valid_position(+Board, +Row, +Col, +Visited)
% Checks if a position on the game board is valid and not visited.
valid_position(Board, Row, Col, Visited):-
    length(Board, NumRows),
    length(Board, NumCols),
    Row >= 1, Row =< NumRows,
    Col >= 1, Col =< NumCols,
    \+ (member((Row-Col), Visited)).

% count_pieces(+Board, +Color, -Count)
% Counts the number of pieces of a specific color on the game board. 
count_pieces(Board, Color, Count) :-
    count_pieces(Board, Color, 0, Count).

% count_pieces(+Rows, +Color, +CurrentCount, -Count)
% Counts the number of pieces of a specific color in a row of the game board.
count_pieces([], _, Count, Count).
count_pieces([Row|Rest], Color, CurrentCount, Count) :-
    count_row(Row, Color, RowCount),
    NewCount is CurrentCount + RowCount,
    count_pieces(Rest, Color, NewCount, Count).

% count_row(+Row, +Color, -Count)
% Counts the number of pieces of a specific color in a row.
count_row([], _, 0).
count_row([Piece|Rest], Color, Count) :-
    (Piece = Color -> RowCount = 1; RowCount = 0),
    count_row(Rest, Color, RestCount),
    Count is RowCount + RestCount.


% ---------- ASCII ARTS ----------

% red_wins
% Displays a victory message for the red player.
red_wins:-
    write(' ____          _  __        ___           _ '), nl,
    write('|  _ \\ ___  __| | \\ \\      / (_)_ __  ___| |'), nl,
    write('| |_) / _ \\/ _` |  \\ \\ / /| | \'_ \\/ __| |'), nl,
    write('|  _ <  __/ (_| |   \\ V  V / | | | | \\__ |_|'), nl,
    write('|_| \\_\\___|\\__,_|    \\_/\\_/  |_|_| |_|___(_)'), nl.

% blue_wins
% Displays a victory message for the blue player.
blue_wins:-
    write(' ____  _             __        ___           _ '), nl,
    write('| __ )| |_   _  ___  \\ \\      / (_)_ __  ___| |'), nl,
    write('|  _ \\| | | | |/ _ \\  \\ \\ / /| | \'_ \\/ __| |'), nl,
    write('| |_) | | |_| |  __/   \\ V  V / | | | | \\__ |_|'), nl,
    write('|____/|_|\\__,_|\\___|    \\_/\\_/  |_|_| |_|___(_)'), nl.

% bounce
% Displays a welcome message when entering the game menu.
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

% bounce_game
% Displays a message when entering the game.
bounce_game:-
     write('  ____                              \n'),                          
     write(' | __ )  ___  _   _ _ __   ___ ___  \n'),
     write(' |  _ \\ / _ \\| | | | |_ \\ / __/ _ \\ \n'),
     write(' | |_) | (_) | |_| | | | | (_|  __/ \n'),
     write(' |____/ \\___/ \\__,_|_| |_|\\___\\___| \n').    


