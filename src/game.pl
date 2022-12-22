:- use_module(library(between)).
:- use_module(library(lists)).

% mergelists(+ListOfLists, ?List)
mergelists([], []).
mergelists([H|T], List) :-
    append(H, List1, List),
    mergelists(T, List1).

/*
% initial_board(+RowNumber, -Row)
initial_board(0, [blank, blank, tent1, head1, head1, tent1, blank, blank]).
initial_board(1, [blank, blank, tent1, head1, head1, tent1, blank, blank]).
initial_board(2, [blank, blank, tent1, tent1, tent1, tent1, blank, blank]).
initial_board(3, [blank, blank, blank, blank, blank, blank, blank, blank]).
initial_board(4, [blank, blank, blank, blank, blank, blank, blank, blank]).
initial_board(5, [blank, blank, tent2, tent2, tent2, tent2, blank, blank]).
initial_board(6, [blank, blank, tent2, head2, head2, tent2, blank, blank]).
initial_board(7, [blank, blank, tent2, head2, head2, tent2, blank, blank]).

% initial_state(-GameState-Player)
initial_state(GameState-Player) :- 
    initial_state(7, GameState),
    Player = human.


% initial_state(+Size, -GameState)

initial_state(Size, [Row]) :-
    Size = 0,
    initial_board(0, Row), !.

initial_state(Size, [Row|GameState]) :-
    Size > 0,
    NSize is Size-1,
    initial_state(NSize, GameState),
    initial_board(Size, Row).
*/

%cell(Row, Col, Piece).
% initial_state_(+Row, +Col, +Size, -Board)
initial_state_(Size, 0, Size, []) :- !.
initial_state_(Row, Size, Size, Board) :-
    NextRow is Row + 1,
    initial_state_(NextRow, 0, Size, Board).

initial_state_(Row, Col, Size, [Cell|Board]) :-
    (between(0, 1, Row), (Col is Size // 2 - 2; Col is Size // 2 + 1);
    Row is 2, Lower is Size // 2 - 2, Upper is Size // 2 + 1, between(Lower, Upper, Col)),
    Cell = cell(Row, Col, t-1), !,
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).

initial_state_(Row, Col, Size, [Cell|Board]) :-
    (Lower is Size - 2, Upper is Size - 1, between(Lower, Upper, Row), (Col is Size // 2 - 2; Col is Size // 2 + 1);
    Row is Size - 3, Lower is Size // 2 - 2, Upper is Size // 2 + 1, between(Lower, Upper, Col)),
    Cell = cell(Row, Col, t-2), !,
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).

initial_state_(Row, Col, Size, [Cell|Board]) :-
    (Row is 0; Row is 1), (Col is Size // 2; Col is Size // 2 - 1),
    Cell = cell(Row, Col, h-1), !,
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).

initial_state_(Row, Col, Size, [Cell|Board]) :-
    (Row is Size - 1; Row is Size - 2), (Col is Size // 2; Col is Size // 2 - 1),
    Cell = cell(Row, Col, h-2), !,
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).

initial_state_(Row, Col, Size, [Cell|Board]) :-
    Cell = cell(Row, Col, empty),
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).

% initial_state(+Size, -GameState)
initial_state(Size, GameState) :-
    initial_state_(0, 0, Size, ListOfCells),
    GameState = [board-ListOfCells, turnPlayer-1, size-Size].


% menu_option(+OptionNumber, -Option)
menu_option(1, play).
menu_option(2, instructions).
menu_option(3, quit).

% display_game(+GameState-Player)
display_game(GameState-Player) :-
    format('~t~1|~t~d~t~10+~t~d~t~10+~t~d~t~10+~t~d~t~10+~t~d~t~10+~t~d~t~10+~t~d~t~10+~t~d~t~10+~n', [0,1,2,3,4,5,6,7]),
    format('~t~2|~`-t~9+~`-t~10+~`-t~10+~`-t~10+~`-t~10+~`-t~10+~`-t~10+~`-t~9+~n', []),
    (foreach(Row, GameState), count(RowNum, 0, _Max) do
        nl, 
        format('~d~t~1||', [RowNum]), 
        format('~t~a~t~10||~t~a~t~10+|~t~a~t~10+|~t~a~t~10+|~t~a~t~10+|~t~a~t~10+|~t~a~t~10+|~t~a~t~10+|~n', Row), 
        nl,
        format('~t~2|~`-t~9+~`-t~10+~`-t~10+~`-t~10+~`-t~10+~`-t~10+~`-t~10+~`-t~9+~n', [])
    ),
    format('Next to move is: ~a.~n', [Player]).

% direction(Direction, RowInc, ColInc)
direction(top, 1, 0).
direction(bottom, -1, 0).
direction(left, 0, -1).
direction(right, 0, 1).
direction(topleft, 1, -1).
direction(topright, 1, 1).
direction(bottomleft,-1, -1).
direction(bottomright, -1, 1).

direction_pieces_(Board, Row-Col, _Player, 0) :- memberchk(cell(Row, Col, t-_P), Board).
direction_pieces_(Board, Row-Col, Player, 1) :- memberchk(cell(Row, Col, h-Player), Board).

direction_pieces(_Board, Size-_Col, Size, _Player, _Dir, 0).
direction_pieces(_Board, _Row-Size, Size, _Player, _Dir, 0).
direction_pieces(_Board, Row-Col, _Size, _Player, _Dir, 0) :- Row < 0 ; Col < 0.

direction_pieces(Board, Row-Col, _Size, Player, Dir, HeadFlag) :- !,
    direction(Dir, RowInc, ColInc),
    NextRow is Row + RowInc, NextCol is Col + ColInc, 
    direction_pieces_(Board, NextRow-NextCol, Player, HeadFlag).

direction_pieces(Board, Row-Col, Size, Player, Dir, HeadFlag) :-
    direction(Dir, RowInc, ColInc),
    NextRow is Row + RowInc, NextCol is Col + ColInc, 
    \+ direction_pieces_(Board, NextRow-NextCol, Player, HeadFlag),
    direction_pieces(Board, NextRow-NextCol, Size, Player, Dir, HeadFlag).

% check if head is in line of sight
    %   check each direction
    %       change to new direction if tentacle is found or all cells are empty
    %       succeed if head is found in any direction
line_of_sight(Board, Row-Col, Size, Player) :-
    setof(HeadFlag, Dir^(direction_pieces(Board, Row-Col, Size, Player, Dir, HeadFlag), HeadFlag = 1), _Flags).

increment_coordinates(Coords, RowInc, ColInc, NewCoords) :-
    (foreach(Row-Col, Coords),
        foreach(NextRow-NextCol, NewCoords), param(RowInc, ColInc) do
            NextRow is Row + RowInc, NextCol is Col + ColInc
    ).

get_avaliable_cells_(Board, Coords, Cells) :-
    (foreach(Row-Col, Coords),
        foreach(Cell, Cells), param(Board) do
            memberchk(cell(Row, Col, empty), Board),
            Cell = cell(Row, Col, empty)
    ).

get_avaliable_cells(_Board, Coords, Size, _Dir, []) :-
    (foreach(Row-Col, Coords), param(Size) do Row < 0 ; Row >= Size ; Col < 0 ; Col >= Size).

get_avaliable_cells(Board, Coords, _Size, Dir, []) :-
    direction(Dir, RowInc, ColInc),
    increment_coordinates(Coords, RowInc, ColInc, NewCoords),
    \+ get_avaliable_cells_(Board, NewCoords, _Cells).

get_avaliable_cells(Board, Coords, Size, Dir, [Cells|T]) :-
    direction(Dir, RowInc, ColInc),
    increment_coordinates(Coords, RowInc, ColInc, NewCoords),
    get_avaliable_cells_(Board, NewCoords, Cells),
    get_avaliable_cells(Board, NewCoords, Size, Dir, T).

avaliable_cells(Board, Coords, Size, ListOfCells) :-
    findall(Cells, (get_avaliable_cells(Board, Coords, Size, _Dir, CellList), mergelists(CellList, Cells)), ListOfListsCells),
    mergelists(ListOfListsCells, ListOfCells).

replace(NewBoard, _Piece, [], NewBoard).
replace(Board, Piece, [Row-Col|Coords], NewBoard) :-
    select(cell(Row, Col, Piece), Board, cell(Row, Col, empty), TempBoard),
    replace(TempBoard, Piece, Coords, NewBoard).

place(NewBoard, _Piece, [], NewBoard).
place(Board, Piece, [Row-Col|Coords], NewBoard) :-
    select(cell(Row, Col, empty), Board, cell(Row, Col, Piece), TempBoard),
    place(TempBoard, Piece, Coords, NewBoard).

% move(+GameState, +Move, -NewGameState)
/*
 Move = [Piece, FromCoords, ToCoords]
        [t, [1-1], [2-3]]
        [h, [1-2,1-3,2-2,2-3], [2-2,2-3,2-4,2-5]] 
*/
move(GameState, Move, NewGameState) :-
    GameState = [board-Board, turnPlayer-Player, size-Size],
    Move = [Piece, FromCoords, ToCoords],

    % Replace current pieces with empty cells
    replace(Board, Piece-Player, FromCoords, TempBoard),

    % Place pieces in target empty cells
    place(TempBoard, Piece-Player, ToCoords, NewBoard),

    NewGameState = [board-NewBoard, turnPlayer-Player, size-Size].

valid_moves(GameState, Player, ListOfMoves) :-
    GameState = [board-Board, _turnPlayer, size-Size],
    findall([AtRow-AtCol, AvailCells], 
        (select(cell(AtRow, AtCol, t-Player), Board, _Res), 
            line_of_sight(Board, AtRow-AtCol, Size, Player),
            avaliable_cells(Board, [AtRow-AtCol], Size, AvailCells)
        ),
        TentacleMoves
    ),
    findall([HeadCoords, AvailCells], 
        (findall(Row-Col, select(cell(Row, Col, h-Player), Board, _Res), HeadCoords),
            avaliable_cells(Board, HeadCoords, Size, AvailCells)
        ),
        HeadMoves
    ),
    append([t-TentacleMoves], [h-HeadMoves], ListOfMoves).

choose_move(GameState, Player, Level, Move) :-
    true.

value(GameState, Player, Value).
  
gameloop(GameState) :-
    game_over(GameState, Winner), !,
    congratulate(Winner).

gameloop(GameState) :-
    choose_move(GameState, Player, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(GameState), !,
    gameloop(NewGameState).

play :-
    initial_state(Size, GameState),
    display_game(GameState).
    gameloop(GameState).

instructions :-
    write('Tako Judo - the timeless sport of octopus wrestling - is'), nl,
    write('an abstract strategy game for 2 to 4 players in which the'), nl,
    write('object is to prevent your opponent(s) from being able to'), nl,
    write('move. Played on a 64-square board (for two players) or a'), nl,
    write('100-square board (for three or four players), each player'), nl,
    write('controls the head and eight tentacles of an octopus. The'), nl,
    write('head takes up a 2x2 square area, and each tentacle takes'), nl,
    write('up one square. Any piece can move like a Chess queen (any'), nl,
    write('distance along an open orthogonal or diagonal line, but'), nl,
    write('without jumping) provided that there is also a clear line'), nl,
    write('of sight between that piece`s original location and the'), nl,
    write('head piece for the octopus to which it belongs. A player'), nl,
    write('whose octopus is no longer able to effectively move its'), nl,
    write('head or any tentacles (either move them at all or only'), nl,
    write('harmlessly move a tentacle back and forth between a couple'), nl,
    write('of squares) is pinned and eliminated from the game. Last'), nl,
    write('octopus to move freely controls the sea and wins the game.'), nl,
    call(menu).

quit :-
    nl.

menu :-
    repeat,
    nl,
    write('Please select a number:'), nl,
    write_menu_list,
    read(OptionNumber),
    (   menu_option(OptionNumber, OptionName)
    ->  write('You selected: '), write(OptionName), nl, !
    ;   write('Not a valid choice, try again...'), nl, fail
    ),
    nl,
    call(OptionName).

write_menu_list :-
    menu_option(N, Name),
    write(N), write('. '), write(Name), nl,
    fail.
    
write_menu_list.