:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).

:- prolog_flag(syntax_errors, _, dec10).

:- op(500, fx, piece).
:- op(550, xfx, from).
:- op(570, xfx, to).

to(from(Tentacle, FromCoords), ToCoords, _GameState, Move) :-
    from(Tentacle, FromCoords, ToCoords, Move).

to(piece(head), ToCoords, [board-Board, turnPlayer-PlayerNum|_], [h, FromCoords, ToCoords]) :-
    findall(Row-Col, select(cell(Row, Col, h-PlayerNum), Board, _Rem), FromCoords).

from(piece(tentacle), FromCoords, ToCoords, [t, [FromCoords], [ToCoords]]).


% mergelists(+ListOfLists, ?List)
mergelists([], []).
mergelists([H|T], List) :-
    append(H, List1, List),
    mergelists(T, List1).

read_number_(X, N) :- 
	peek_code(Input),
	Input == 10,
	X = 0,
	N = 0,
	!.

read_number_(X, N) :-
	get_code(Input),
	read_number_(X1, N1),
	X2 is Input-"0",
	X is X2*(10^N1)+X1,
	N is N1+1.

% read_number(-Number)
read_number(Number) :-						
	read_number_(Number, _N),
    skip_line.

read_string_(X) :- 
	peek_code(Input),
	Input == 10,
	X = [],
	!.

read_string_([Input|X]) :-
	get_code(Input),
	read_string_(X).

% read_string(-String)
read_string(String) :-						
	read_string_(String),
    skip_line.

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
    initial_state_(0, 0, Size, Board),
    GameState = [board-Board, turnPlayer-1, size-Size, _Players, _Levels, turnCounter-1].


% menu_option(+OptionNumber, -Option)
menu_option(1, start).
menu_option(2, instructions).
menu_option(3, quit).

cellsize(5).
player_amount(2).

piece_string(empty, '').
piece_string(Piece-Player, [PieceCode, PlayerCode]) :- 
    char_code(Piece, PieceCode), 
    PlayerCode is 48 + Player.

display_colnums(CellSize, BoardSize) :-
    ColLimit is BoardSize - 1,
    format('~t~2|', []),
    (for(Col, 0, ColLimit), param(CellSize) do Tab is (Col + 1) * CellSize + 2, format('~t~t~d~t~*|', [Col, Tab])),
    format('~n', []).

display_cells([], _Size, _Player, RowSize, _RowNum, _CellSize) :- format('~t~2|~1+~`-t~*+~n', RowSize).
display_cells(Board, Size, Player, RowSize, RowNum, CellSize) :-
    NextRowNum is RowNum - 1,
    length(CellRow, Size),
    append(CellRow, RemBoard, Board),
    reverse(CellRow, InvCellRow),
    ((RowNum = Size) -> format('~t~2|~1+~`-t~*+~n', RowSize) ; format('~t~2||~1+~`-t~*+|~n', RowSize)),
    (foreach(Cell, InvCellRow), for(Col, 1, Size), param(NextRowNum, Size, CellSize) do 
        arg(3, Cell, Piece), piece_string(Piece, String), 
        ((Col = 1)
        -> Tab is (Col * CellSize), format('~d~2||~t~s~t~*+|', [NextRowNum, String, Tab]) 
        ; Tab is (Col * CellSize) + 2, format('~t~s~t~*||', [String, Tab]))
    ),
    ((RowNum = Size) -> format('~*|~t~d~2+~t~8+TURN PLAYER:  ~d~n', [RowSize, NextRowNum, Player]) ; format('~*|~t~d~2+~n', [RowSize, NextRowNum])),
    display_cells(RemBoard, Size, Player, RowSize, NextRowNum, CellSize).

% display_game(+GameState)
display_game(GameState) :-
    GameState = [board-Board, turnPlayer-PlayerNum, size-Size|_],
    cellsize(CellSize),
    RowSize is (Size * CellSize) - 1,
    display_colnums(CellSize, Size),
    reverse(Board, RevBoard),
    display_cells(RevBoard, Size, PlayerNum, RowSize, Size, CellSize),
    display_colnums(CellSize, Size).

% direction(Direction, RowInc, ColInc)
direction(top, 1, 0).
direction(bottom, -1, 0).
direction(left, 0, -1).
direction(right, 0, 1).
direction(topleft, 1, -1).
direction(topright, 1, 1).
direction(bottomleft,-1, -1).
direction(bottomright, -1, 1).

seek_head_(Board, Row-Col, _Player, 0) :- memberchk(cell(Row, Col, t-_P), Board).
seek_head_(Board, Row-Col, Player, 1) :- memberchk(cell(Row, Col, h-Player), Board).

seek_head(_Board, Size-_Col, Size, _Player, _Dir, 0) :- !.
seek_head(_Board, _Row-Size, Size, _Player, _Dir, 0) :- !.
seek_head(_Board, Row-Col, _Size, _Player, _Dir, 0) :- (Row < 0 ; Col < 0), !.


seek_head(Board, Row-Col, _Size, Player, Dir, HeadFlag) :-
    direction(Dir, RowInc, ColInc),
    NextRow is Row + RowInc, NextCol is Col + ColInc,
    seek_head_(Board, NextRow-NextCol, Player, HeadFlag).

seek_head(Board, Row-Col, Size, Player, Dir, HeadFlag) :-
    direction(Dir, RowInc, ColInc),
    NextRow is Row + RowInc, NextCol is Col + ColInc, 
    \+ seek_head_(Board, NextRow-NextCol, Player, HeadFlag),
    seek_head(Board, NextRow-NextCol, Size, Player, Dir, HeadFlag).

% check if head is in line of sight
    %   check each direction
    %       change to new direction if tentacle is found or all cells are empty
    %       succeed if head is found in any direction
head_in_sight(Board, Row-Col, Size, Player) :-
    setof(HeadFlag, Dir^(seek_head(Board, Row-Col, Size, Player, Dir, HeadFlag), HeadFlag = 1), _Flags).

increment_coordinates(Coords, RowInc, ColInc, NewCoords) :-
    (foreach(Row-Col, Coords),
        foreach(NextRow-NextCol, NewCoords), param(RowInc, ColInc) do
            NextRow is Row + RowInc, NextCol is Col + ColInc
    ).

available_cells(Board, t-_Player, Coords, Cells) :-
    (foreach(Row-Col, Coords),
        foreach(Cell, Cells), param(Board) do
            memberchk(cell(Row, Col, empty), Board),
            Cell = Row-Col
    ).

available_cells(Board, h-Player, Coords, Cells) :-
    (foreach(Row-Col, Coords),
        foreach(Cell, Cells), param(Board) do
            (memberchk(cell(Row, Col, empty), Board);
            memberchk(cell(Row, Col, h-Player), Board)),
            Cell = Row-Col
    ).

dir_available_cells(_Board, _Piece, Coords, Size, _Dir, []) :-
    (foreach(Row-Col, Coords), param(Size) do Row < 0 ; Row >= Size ; Col < 0 ; Col >= Size).

dir_available_cells(Board, Piece, Coords, _Size, Dir, []) :-
    direction(Dir, RowInc, ColInc),
    increment_coordinates(Coords, RowInc, ColInc, NewCoords),
    \+ available_cells(Board, Piece, NewCoords, _Cells).

dir_available_cells(Board, Piece, Coords, Size, Dir, [Cells|T]) :-
    direction(Dir, RowInc, ColInc),
    increment_coordinates(Coords, RowInc, ColInc, NewCoords),
    available_cells(Board, Piece, NewCoords, Cells),
    dir_available_cells(Board, Piece, NewCoords, Size, Dir, T).

list_available_cells(Board, Piece, Coords, Size, ListOfCells) :-
    findall(Cells, (dir_available_cells(Board, Piece, Coords, Size, _Dir, Cells)), ListOfListsCells),
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
initial_state(8, GS), !, move(GS, [t, [0-2], [0-0]], NGS), display_game(NGS), NGS = [board-B, _, size-S], FGS = [board-B, turnPlayer-2, size-S, choose_move(FGS, 2-computer, 2, Move). 

 Move = [Piece, FromCoords, ToCoords]
        [t, [1-1], [2-3]]
        [h, [1-2,1-3,2-2,2-3], [2-2,2-3,2-4,2-5]] 
*/
move(GameState, Move, NewGameState) :-
    GameState = [board-Board, turnPlayer-Player, size-Size, Players, Levels|_],
    Move = [Piece, FromCoords, ToCoords],

    valid_moves(GameState, Player, ListOfMoves),
    memberchk(Move, ListOfMoves),

    % Replace current pieces with empty cells
    replace(Board, Piece-Player, FromCoords, TempBoard),

    % Place pieces in target empty cells
    place(TempBoard, Piece-Player, ToCoords, NewBoard),

    NewGameState = [board-NewBoard, turnPlayer-_Player, size-Size, Players, Levels, turnCounter-_Counter].

construct_moves(Piece, FromCoords, ListToCoords, Moves) :-
    (foreach(ToCoords, ListToCoords),
        foreach(Move, Moves), param(Piece, FromCoords) do
            Move = [Piece, FromCoords, ToCoords]
    ).

valid_tentacle_moves(Board, Player, Size, TentacleMoves) :-
    findall(TentacleMoves, (
            select(cell(Row, Col, t-Player), Board, _Res), 
            head_in_sight(Board, Row-Col, Size, Player),
            list_available_cells(Board, t-Player, [Row-Col], Size, AvailCells),
            dif(AvailCells, []),
            construct_moves(t, [Row-Col], AvailCells, TentacleMoves)
        ),
        ListOfListsTentacleMoves
    ),
    mergelists(ListOfListsTentacleMoves, TentacleMoves).
    
valid_head_moves(Board, Player, Size, HeadMoves) :-
    findall(HeadMoves, (
            findall(Row-Col, select(cell(Row, Col, h-Player), Board, _Res), HeadCoords),
            list_available_cells(Board, h-Player, HeadCoords, Size, AvailCells),
            dif(AvailCells, []),
            construct_moves(h, HeadCoords, AvailCells, HeadMoves)
        ),
        ListOfListsHeadMoves
    ),
    mergelists(ListOfListsHeadMoves, HeadMoves).

valid_moves(GameState, Player, ListOfMoves) :-
    GameState = [board-Board, _turnPlayer, size-Size|_],
    valid_tentacle_moves(Board, Player, Size, TentacleMoves),
    valid_head_moves(Board, Player, Size, HeadMoves),
    append(TentacleMoves, HeadMoves, ListOfMoves).

%choose_move(+GameState, +Player, +Level, -Move)
choose_move(GameState, PlayerNum-human, _Level, Move) :-
    repeat,
        format('Player ~d to move:~n', [PlayerNum]),
        read(Input),
        (call(Input, GameState, Move)
            -> !
            ; write('Error: Invalid syntax.'), nl, fail
        ).
        
choose_move(GameState, PlayerNum-computer, 1, Move) :-
    valid_moves(GameState, PlayerNum, ListOfMoves),
    random_select(Move, ListOfMoves, _Rest).

choose_move(GameState, PlayerNum-computer, 2, Move) :-
    valid_moves(GameState, PlayerNum, ListOfMoves),
    setof(Value-Mv, NewGameState^(
            member(Mv, ListOfMoves),
            move(GameState, Mv, NewGameState),
            value(NewGameState, PlayerNum, Value)
        ),
        [Min-Mv|Moves]
    ),
    best_moves(Min, [Min-Mv|Moves], BestMoves),
    random_select(Move, BestMoves, _Rest).

% best_moves(+Moves, -BestMoves)
best_moves(_Min, [], []).
best_moves(Min, [Value-_Move|_Moves], []) :-
	Min < Value,
	!.
	
best_moves(Min, [Value-Move|Moves], [Move|BestMoves]) :-
	Min >= Value,
	best_moves(Min, Moves, BestMoves).

eval_tentacles(_Board, [], _Size, 0).
eval_tentacles(Board, [PlayerNum-Row-Col|Tentacles], Size, Value) :-
    eval_tentacles(Board, Tentacles, Size, Value),
    head_in_sight(Board, Row-Col, Size, PlayerNum).

eval_tentacles(Board, [PlayerNum-Row-Col|Tentacles], Size, Value) :-
    eval_tentacles(Board, Tentacles, Size, Value1),
    \+ head_in_sight(Board, Row-Col, Size, PlayerNum),
    Value is Value1 + 1.

value(GameState, Player, Value) :-
    GameState = [board-Board, _turnPlayer, size-Size|_],
    findall(PlayerNum-Row-Col, (select(cell(Row, Col, t-PlayerNum), Board, _Rem), PlayerNum =\= Player), EnemyTentacles),
    findall(Player-Row-Col, (select(cell(Row, Col, t-Player), Board, _Rem)), PlayerTentacles),
    eval_tentacles(Board, EnemyTentacles, Size, Score1),
    eval_tentacles(Board, PlayerTentacles, Size, Score2),
    valid_head_moves(Board, Player, Size, PlayerHeadMoves),
    (foreach(HeadMove, PlayerHeadMoves),
        count(I, 1, Score3) do
            true
    ),
    valid_head_moves(Board, 2, Size, EnemyHeadMoves),
    (foreach(HeadMove, EnemyHeadMoves),
        count(I, 1, Score4) do
            true
    ),
    Value is - Score1 + Score2 - Score3 + Score4.

game_over(GameState, Winner) :-
    GameState = [board-Board, turnPlayer-Player, size-Size, _Players , _Levels, turnCounter-TurnCounter],
    TurnCounter =\= 1,
    (valid_head_moves(Board, Player, Size, []); valid_tentacle_moves(GameState, Player, Size, [])),
    next_player(Player, Winner).

next_player(Player, NextPlayer) :-
    player_amount(Amount),
    NextPlayer is 1 + (Player mod Amount).

gameloop(GameState) :-
    game_over(GameState, Winner), !,
    congratulate(Winner).

gameloop(GameState) :-
    repeat,
    /*write('Select type of pieace (Tentacle-t, Head-h):'), nl,
    read(Piece),
    (   Piece = 'h'
    ->  write('Row:'), nl,
        read(Row),
        write('Col:'), nl,
        read(Col),
        write('New Row:'), nl,
        read(NewRow),
        write('New Col:'), nl,
        read(NewCol)
    ;   write('Top Row:'), nl,
        read(TopRow),
        write('Left Col:'), nl,
        read(LeftCol),
        DownRow is TopRow-1,
        RightCol is LeftCol+1,
        write('New Top Row:'), nl,
        read(NewTopRow),
        write('New Left Col:'), nl,
        read(NewLeftCol),
        NewDownRow is NewTopRow-1,
        NewRightCol is NewLeftCol+1
    ),
    nl,*/
        display_game(GameState),
        GameState = [_Board, turnPlayer-Player, _Size, players-Players, pc_levels-Levels, turnCounter-TurnCounter],
        member(Player-Type, Players),
        (Type = computer -> member(Player-Level, Levels) ; true),
        choose_move(GameState, Player-Type, Level, Move),
        move(GameState, Move, NewGameState),
        next_player(Player, NextPlayer),
        NextTurnCounter is TurnCounter + 1,
        prolog_flag(debugging, _, trace),
        NewGameState = [_, turnPlayer-NextPlayer, _Size, _Players, _Levels, turnCounter-NextTurnCounter],
        !,
        gameloop(NewGameState).

% size_option(+OptionNumber, -Option)
size_option(1, 8).
size_option(2, 10).

play :-
    repeat,
        format('~n~`*t~1|~`-t Tako Judo ~`-t~*+~`*t~1+~n', [50]),
        format('~`|t~1|~t~*+~`|t~1+~n', [50]),
        format('~`|t~1|~t start ~t~*+~`|t~1+~n', [50]),
        format('~`|t~1|~t instructions ~t~*+~`|t~1+~n', [50]),
        format('~`|t~1|~t quit ~t~*+~`|t~1+~n', [50]),
        format('~`*t~1|~`-t~*+~`*t~1+~n', [50]),
        write('Choose an option:'), nl,
        read(Input),
        skip_line,
        (Input = quit -> !, fail ; true),
        call(Input),
        fail.

start :-
    % select board size
    board_size_menu(BoardSize),
    % select player configuration
    player_config_menu(Players),
    pc_level_menu(Players, PCLevels),
    % start game loop
    initial_state(BoardSize, GameState),
    GameState = [_Board, _TurnPlayer, _Size, players-Players, pc_levels-PCLevels, _turnCounter], !,
    gameloop(GameState).

board_sizes([8, 10]).
pc_levels([1, 2]).
player_configs(["H/H", "H/PC", "PC/H", "PC/PC"]).
player_config("H/H", [1-human, 2-human]).
player_config("H/PC", [1-human, 2-computer]).
player_config("PC/H", [1-computer, 2-human]).
player_config("PC/PC", [1-computer, 2-computer]).

board_size_menu(BoardSize) :-
    board_sizes(BoardSizes),
    repeat,
        format('~n~`*t~1|~`-t Sizes ~`-t~*+~`*t~1+~n', [30]),
        format('~`|t~1|~t~*+~`|t~1+~n', [30]),
        format('~`|t~1|~t  >8 (8x8) ~t   >10 (10x10) ~t~*+~`|t~1+~n', [30]),
        format('~`*t~1|~`-t~*+~`*t~1+~n', [30]),
        write('Choose a size:'), nl,
        read_number(BoardSize),
        (memberchk(BoardSize, BoardSizes) -> !, true ; write('Invalid size.'), nl, fail).

player_config_menu(Players) :-
    player_configs(PlayerConfigs),
    repeat,
        format('~n~`*t~1|~`-t Player Config ~`-t~*+~`*t~1+~n', [30]),
        format('~`|t~1|~t~*+~`|t~1+~n', [30]),
        format('~`|t~1|~t>H/H~t  >PC/PC~t~*+~`|t~1+~n', [30]),
        format('~`|t~1|~t>H/PC~t>PC/H~t~*+~`|t~1+~n', [30]),
        format('~`*t~1|~`-t~*+~`*t~1+~n', [30]),
        write('Choose a configuration:'), nl,
        read_string(PlayerConfig),
        (memberchk(PlayerConfig, PlayerConfigs) 
        -> !, player_config(PlayerConfig, Players) ; write('Invalid configuration.'), nl, fail).

pc_level_menu(Players, PCLevels) :-
    pc_levels(Levels),
    findall(PlayerNum, member(PlayerNum-computer, Players), PCPlayers),
    (foreach(PCPlayer, PCPlayers),
        foreach(PCLevel, PCLevels), param(Levels) do
            repeat,
                format('~n~`*t~1|~`-t PC-~d Levels ~`-t~*+~`*t~1+~n', [PCPlayer, 30]),
                format('~`|t~1|~t~*+~`|t~1+~n', [30]),
                format('~`|t~1|~t>1~t>2~t~*+~`|t~1+~n', [30]),
                format('~`*t~1|~`-t~*+~`*t~1+~n', [30]),
                write('Choose a difficulty level:'), nl,
                read_number(Level),
                (memberchk(Level, Levels) -> !, PCLevel = PCPlayer-Level ; write('Invalid level.'), nl, fail)
    ) ; PCLevels = [].
/*
play :-
    repeat,
    write('Please select the size of your board (choose option number):'), nl,
    write_size_list,
    read_number(OptionNumber), skip_line,
    (   size_option(OptionNumber, OptionName)
    ->  write('You selected: '), write(OptionName), nl, !
    ;   write('Not a valid choice, try again...'), nl, fail
    ),
    nl,
    initial_state(OptionName, GameState),
    gameloop(GameState).
*/

write_size_list :-
    size_option(N, Name),
    write(N), write('. '), write(Name), nl,
    fail.
    
write_size_list.

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
    write('octopus to move freely controls the sea and wins the game.'), nl.

/*
quit :-
    nl.
*/

menu :-
    repeat,
    nl,
    write('Please select a number:'), nl,
    write_menu_list,
    read_string(Input), skip_line,
    atom_codes(OptionName, Input),
    /*read(OptionNumber),
    (   menu_option(OptionNumber, OptionName)
    ->  write('You selected: '), write(OptionName), nl, !
    ;   write('Not a valid choice, try again...'), nl, fail
    ),
    nl,*/
    call(OptionName).


write_menu_list :-
    menu_option(N, Name),
    write(N), write('. '), write(Name), nl,
    fail.
    
write_menu_list.