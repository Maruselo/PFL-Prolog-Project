/* Library loading 
    library(between) - Constraint checks
    library(lists)   - List manipulation
    library(random)  - Randomness
*/
:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).


% Set 'syntax_errors' flag to 'dec10' so the predicate read/1 reattemps a read after failure
:- prolog_flag(syntax_errors, _, dec10).


/* Setting custom operators for piece movement syntax 
    Usage: piece <piece-type> from <piece-current-coordinates> to <piece-target-coordinates>
    
    For the head piece, user should omit the 'from' operator as the head current coordinates are automatically calculated
*/
:- op(500, fx, piece).
:- op(550, xfx, from).
:- op(570, xfx, to).

to(from(Tentacle, FromCoords), ToCoords, _GameState, Move) :-
    from(Tentacle, FromCoords, ToCoords, Move).

to(piece(head), ToCoords, [board-Board, turnPlayer-PlayerNum|_], [h, FromCoords, ToCoords]) :-
    findall(Row-Col, select(cell(Row, Col, h-PlayerNum), Board, _Rem), FromCoords).

from(piece(tentacle), FromCoords, ToCoords, [t, [FromCoords], [ToCoords]]).


/* Utility functions */
% mergelists(+ListOfLists, ?List) - Merge a list of lists into a single list
mergelists([], []).
mergelists([H|T], List) :-
    append(H, List1, List),
    mergelists(T, List1).


% read_number(-Number) - Read an integer from the input stream.
read_number(Number) :-						
	read_number_(Number, _N),
    skip_line.

% Auxiliary predicate for read_number/1
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


% read_string(-String) - Read a string from the input stream.
read_string(String) :-						
	read_string_(String),
    skip_line.

% Auxiliary predicate for read_string/1
read_string_(X) :- 
	peek_code(Input),
	Input == 10,
	X = [],
	!.

read_string_([Input|X]) :-
	get_code(Input),
	read_string_(X).


/* Game functions */
% initial_state(+Size, -GameState)
% Takes the size of the board as input, outputs the term GameState containing information about the game such as:
% the initial state of the board, size of the board, player to move and turn counter.
initial_state(Size, GameState) :-
    initial_state_(0, 0, Size, Board),
    GameState = [board-Board, turnPlayer-1, size-Size, turnCounter-1, moveLog-[]].


% initial_state_(+Row, +Col, +Size, -Board) - Auxiliary predicate for initial_state/2
initial_state_(Size, 0, Size, []) :- !.
initial_state_(Row, Size, Size, Board) :-
    NextRow is Row + 1,
    initial_state_(NextRow, 0, Size, Board).

% Checks if the current cell should contain a player 1 tentacle
initial_state_(Row, Col, Size, [Cell|Board]) :-
    (between(0, 1, Row), (Col is Size // 2 - 2; Col is Size // 2 + 1);
    Row is 2, Lower is Size // 2 - 2, Upper is Size // 2 + 1, between(Lower, Upper, Col)),
    Cell = cell(Row, Col, t-1), !,
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).

% Checks if the current cell should contain a player 2 tentacle
initial_state_(Row, Col, Size, [Cell|Board]) :-
    (Lower is Size - 2, Upper is Size - 1, between(Lower, Upper, Row), (Col is Size // 2 - 2; Col is Size // 2 + 1);
    Row is Size - 3, Lower is Size // 2 - 2, Upper is Size // 2 + 1, between(Lower, Upper, Col)),
    Cell = cell(Row, Col, t-2), !,
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).

% Checks if the current cell should contain a player 1 head
initial_state_(Row, Col, Size, [Cell|Board]) :-
    (Row is 0; Row is 1), (Col is Size // 2; Col is Size // 2 - 1),
    Cell = cell(Row, Col, h-1), !,
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).

% Checks if the current cell should contain a player 2 head
initial_state_(Row, Col, Size, [Cell|Board]) :-
    (Row is Size - 1; Row is Size - 2), (Col is Size // 2; Col is Size // 2 - 1),
    Cell = cell(Row, Col, h-2), !,
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).

% Assigns an empty cell at target coordinates.
initial_state_(Row, Col, Size, [Cell|Board]) :-
    Cell = cell(Row, Col, empty),
    NextCol is Col + 1,
    initial_state_(Row, NextCol, Size, Board).



cellsize(5).    % Defines the size of a cell in the board's display

% piece_string(+Piece, -String) - Takes a player piece in term representation and converts it into its string representation
piece_string(empty, '').
piece_string(Piece-Player, [PieceCode, PlayerCode]) :- 
    char_code(Piece, PieceCode), 
    PlayerCode is 48 + Player.

% display_colnums(+CellSize, +BoardSize) - Prints the column numbers given a board size, spaced by the size of a cell.
display_colnums(CellSize, BoardSize) :-
    ColLimit is BoardSize - 1,
    format('~t~2|', []),
    (for(Col, 0, ColLimit), param(CellSize) do Tab is (Col + 1) * CellSize + 2, format('~t~t~d~t~*|', [Col, Tab])),
    format('~n', []).

% display_board(+Board, +Size, +Player, +TurnCounter, +RowSize, +RowNum, +CellSize)
% Displays the board, current turn and current player to move
display_board([], _Size, _Player, _TurnCounter, RowSize, _RowNum, _CellSize) :- format('~t~2|~1+~`-t~*+~n', RowSize).
display_board(Board, Size, Player, TurnCounter, RowSize, RowNum, CellSize) :-
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
    ((RowNum = Size) -> format('~*|~t~d~2+~t~8+TURN PLAYER:   ~d~n', [RowSize, NextRowNum, Player]) ;
        ((RowNum is Size - 1) -> format('~*|~t~d~2+~t~8+TURN COUNTER:  ~d~n', [RowSize, NextRowNum, TurnCounter]) ; format('~*|~t~d~2+~n', [RowSize, NextRowNum]))),
    display_board(RemBoard, Size, Player, TurnCounter, RowSize, NextRowNum, CellSize).

% display_game(+GameState) - Displays the current state of the game
display_game(GameState) :-
    GameState = [board-Board, turnPlayer-PlayerNum, size-Size, turnCounter-TurnCounter|_],
    cellsize(CellSize),
    RowSize is (Size * CellSize) - 1,
    display_colnums(CellSize, Size),
    reverse(Board, RevBoard),   % Reverse the board list so it displays its cells with coordinates (0,0) at the bottom left
    display_board(RevBoard, Size, PlayerNum, TurnCounter, RowSize, Size, CellSize),
    display_colnums(CellSize, Size), nl.

% direction(+Direction, -RowInc, -ColInc) - Returns the row and column increments for a given direction
direction(top, 1, 0).
direction(bottom, -1, 0).
direction(left, 0, -1).
direction(right, 0, 1).
direction(topleft, 1, -1).
direction(topright, 1, 1).
direction(bottomleft,-1, -1).
direction(bottomright, -1, 1).

% Auxiliary function for seek_head/6
seek_head_(Board, Row-Col, _Player, 0) :- memberchk(cell(Row, Col, t-_P), Board).
seek_head_(Board, Row-Col, Player, 1) :- memberchk(cell(Row, Col, h-Player), Board).

% seek_head(+Board, +Row-Col, +Size, +Player, +Dir, -HeadFlag)
% Seeks the player's head in the given direction.
% Returns in HeadFlag 0 if the head wasn't found, 1 otherwise.
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

% head_in_sight(+Board, +Row-Col, +Size, +Player)
% Predicate that checks if the player's head is in sight from the given coordinates in Row-Col
head_in_sight(Board, Row-Col, Size, Player) :-
    setof(HeadFlag, Dir^(seek_head(Board, Row-Col, Size, Player, Dir, HeadFlag), HeadFlag = 1), _Flags).

% increment_coordinates(+Coords, +RowInc, +ColInc, -NewCoords)
% Takes a set of coordinates, the row increment, the column increments and returns a new set of incremented coordinates
increment_coordinates(Coords, RowInc, ColInc, NewCoords) :-
    (foreach(Row-Col, Coords),
        foreach(NextRow-NextCol, NewCoords), param(RowInc, ColInc) do
            NextRow is Row + RowInc, NextCol is Col + ColInc
    ).

% avaliable_cells(+Board, +Piece, +Coords, -Cells)
% Returns in Cells a list of avaliable cells where the piece can move into.
% For tentacles, only empty cells need to be checked
% For heads, both empty cells and the own player's head cells need to be checked
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

% dir_available_cells(+Board, +Piece, +Coords, +Size, +Dir, -ListOfCells)
% Returns all the avaliable cells in the given direction
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

% list_available_cells(+Board, +Piece, +Coords, +Size, -ListOfCells)
% Starting from the coordinates in Coords, going through all possible directions, returns all available cells within reach
list_available_cells(Board, Piece, Coords, Size, ListOfCells) :-
    findall(Cells, (dir_available_cells(Board, Piece, Coords, Size, _Dir, Cells)), ListOfListsCells),
    mergelists(ListOfListsCells, ListOfCells).

% replace(+Board, +Piece, +FromCoords, -NewBoard)
% Replaces cell at target coordinates FromCoords with the given piece Piece for an empty cell
replace(NewBoard, _Piece, [], NewBoard).
replace(Board, Piece, [Row-Col|Coords], NewBoard) :-
    select(cell(Row, Col, Piece), Board, cell(Row, Col, empty), TempBoard),
    replace(TempBoard, Piece, Coords, NewBoard).

% place(+NewBoard, +Piece, +ToCoords, -NewBoard)
% Places a cell at target coordinates ToCoords with the given piece Piece by replacing am empty cell at the same coordinates.
place(NewBoard, _Piece, [], NewBoard).
place(Board, Piece, [Row-Col|Coords], NewBoard) :-
    select(cell(Row, Col, empty), Board, cell(Row, Col, Piece), TempBoard),
    place(TempBoard, Piece, Coords, NewBoard).

% move(+GameState, +Move, -NewGameState)
% Validates and executes a move Move in the current gamestate GameState, returning a new gamestate
move(GameState, Move, NewGameState) :-
    GameState = [board-Board, turnPlayer-Player, size-Size, _turnCounter, moveLog-MoveLog],
    Move = [Piece, FromCoords, ToCoords],

    % Move verification
    valid_moves(GameState, Player, ListOfMoves),
    (\+ memberchk(Move, ListOfMoves) -> write('Invalid move!'), nl, fail ; true),

    % Replace current pieces with empty cells
    replace(Board, Piece-Player, FromCoords, TempBoard),

    % Place pieces in target empty cells
    place(TempBoard, Piece-Player, ToCoords, NewBoard),

    NewGameState = [board-NewBoard, turnPlayer-_Player, size-Size, turnCounter-_Counter, moveLog-MoveLog].

% construct_moves(+Piece, +FromCoords, +ListToCoords, -Moves)
% Takes the piece Piece to move, its current coordinates, its list of possible target coordinates
% and constructs the list Moves with all possible moves
construct_moves(Piece, FromCoords, ListToCoords, Moves) :-
    (foreach(ToCoords, ListToCoords),
        foreach(Move, Moves), param(Piece, FromCoords) do
            Move = [Piece, FromCoords, ToCoords]
    ).

% valid_tentacle_moves(+Board, +Player, +Size, -TentacleMoves)
% Given a player and current state of the board, and its size, returns a list
% of all possible moves for the player's tentacles
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

% valid_head_moves(+Board, +Player, +Size, -HeadMoves)
% Given a player and current state of the board, and its size, returns a list
% of all possible moves for the player's head
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


% valid_moves(+GameState, +Player, -ListOfMoves)
% Given the current gamestate and the current player to move, returns a list of all valid moves
% of both the player's head and tentacles 
valid_moves(GameState, Player, ListOfMoves) :-
    GameState = [board-Board, _turnPlayer, size-Size|_],
    valid_tentacle_moves(Board, Player, Size, TentacleMoves),
    valid_head_moves(Board, Player, Size, HeadMoves),
    append(TentacleMoves, HeadMoves, ListOfMoves).


%choose_move(+GameState, +Player, +Level, -Move)
% Predicate that receives the current game state, player to move and AI level and returns the Move to be performed
% AI - computes the move to be performed based on its level. 1 - random move; 2 - best move
% Human - prompts the user to input a move
choose_move(GameState, PlayerNum-human, _Level, Move) :-
    repeat,
        format('Player ~d to move:~n', [PlayerNum]),
        write('Move syntax:'), nl,
        write('- piece tentacle from <current-coordinates> to <target-coordinates>.'), nl,
        write('- piece head to [<target-coordinates>]'), nl,
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
    %write([Min-Mv|Moves]), nl,
    best_moves(Min, [Min-Mv|Moves], BestMoves),
    random_select(Move, BestMoves, _Rest).

% best_moves(+Moves, -BestMoves)
% Receives a list of moves Moves, and returns a list of the best moves
% Each move has an associated value, the best moves are those with the lowest value
best_moves(_Min, [], []).
best_moves(Min, [Value-_Move|_Moves], []) :-
	Min < Value,
	!.
	
best_moves(Min, [Value-Move|Moves], [Move|BestMoves]) :-
	Min >= Value,
	best_moves(Min, Moves, BestMoves).

% eval_tentacles(+Board, +Tentacles, +Size, -Value)
% Evaluates the tentacles of a player, scoring more the more tentacles have the head in sight.
eval_tentacles(_Board, [], _Size, 0).
eval_tentacles(Board, [PlayerNum-Row-Col|Tentacles], Size, Value) :-
    eval_tentacles(Board, Tentacles, Size, Value),
    head_in_sight(Board, Row-Col, Size, PlayerNum).

eval_tentacles(Board, [PlayerNum-Row-Col|Tentacles], Size, Value) :-
    eval_tentacles(Board, Tentacles, Size, Value1),
    \+ head_in_sight(Board, Row-Col, Size, PlayerNum),
    Value is Value1 + 1.

% value(+GameState, +Player, -Value)
% Given a gamestate and player to move, evaluates the gamestate for the player and assigns it a value.
value(GameState, Player, Value) :-
    GameState = [board-Board, _turnPlayer, size-Size|_],
    findall(PlayerNum-Row-Col, (select(cell(Row, Col, t-PlayerNum), Board, _Rem), PlayerNum =\= Player), EnemyTentacles),
    findall(Player-Row-Col, (select(cell(Row, Col, t-Player), Board, _Rem)), PlayerTentacles),
    eval_tentacles(Board, EnemyTentacles, Size, EnemyTentVal),
    eval_tentacles(Board, PlayerTentacles, Size, PlayerTentVal),
    valid_head_moves(Board, Player, Size, PlayerHeadMoves),
    (foreach(HeadMove, PlayerHeadMoves),
        count(I, 1, PlayerHeadVal) do
            true
    ),
    next_player(Player, NextPlayer),
    valid_head_moves(Board, NextPlayer, Size, EnemyHeadMoves),
    (foreach(HeadMove, EnemyHeadMoves),
        count(I, 1, EnemyHeadVal) do
            true
    ),
    Value is - 0.8 * EnemyTentVal + PlayerTentVal - PlayerHeadVal + EnemyHeadVal.

% game_over(+GameState, +Winner)
% Predicate that evaluates if the gamestate has ended, returning the winning player
game_over(GameState, Winner) :-
    GameState = [board-Board, turnPlayer-Player, size-Size, turnCounter-TurnCounter, moveLog-MoveLog],
    valid_moves(GameState, Player, []),
    %TurnCounter =\= 1,
    %(valid_head_moves(Board, Player, Size, []); valid_tentacle_moves(GameState, Player, Size, [])),
    next_player(Player, Winner).

% congratulate(+Winner)
% Displays a victory message after a game over
congratulate(Winner) :-
    format('No more moves can be played. Player ~d wins!~n', [Winner]).

% next_player(+Player, -NextPlayer)
% Given the current player, returns the next player to make a move
next_player(Player, NextPlayer) :-
    player_amount(Amount),
    NextPlayer is 1 + (Player mod Amount).

% gameloop(+GameState, +Player, +Levels)
% Main game loop. Checks for game over scenario in every iteration, and otherwise
% follows a sequence of: get the current player's move, execute it, calculate next player, increment turn counter and loop again with the new game state
gameloop(GameState, _Players, _Levels) :-
    game_over(GameState, Winner), !,
    congratulate(Winner).

gameloop(GameState, Players, Levels) :-
    repeat,
        display_game(GameState),
        GameState = [_Board, turnPlayer-Player, _Size, turnCounter-TurnCounter|_],
        member(Player-Type, Players),
        (Type = computer -> member(Player-Level, Levels) ; true), 
        choose_move(GameState, Player-Type, Level, Move),
        move(GameState, Move, NewGameState),
        next_player(Player, NextPlayer),
        NextTurnCounter is TurnCounter + 1,
        NewGameState = [_NewBoard, turnPlayer-NextPlayer, _Size, turnCounter-NextTurnCounter|_],
        !,
        gameloop(NewGameState, Players, Levels).

% play/0
% Initial predicate, displays the main game menu
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

% start/0
% Game configuration menu, prompts user for board size, player configuration and AI level
start :-
    % select board size
    board_size_menu(BoardSize),
    % select player configuration
    player_config_menu(Players),
    pc_level_menu(Players, PCLevels),
    % start game loop
    initial_state(BoardSize, GameState),
    !,
    gameloop(GameState, Players, PCLevels).

% Game values
board_sizes([8, 10]).
pc_levels([1, 2]).
player_amount(2).
player_configs(["H/H", "H/PC", "PC/H", "PC/PC"]).
player_config("H/H", [1-human, 2-human]).
player_config("H/PC", [1-human, 2-computer]).
player_config("PC/H", [1-computer, 2-human]).
player_config("PC/PC", [1-computer, 2-computer]).

% board_size_menu(-BoardSize)
% Prompts the user for a board size and returns the user chosen size
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

% player_config_menu(-PlayerConfigs)
% Prompts the user for a player configuration and returns the user chosen configuration
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

% pc_level_menu(+Players, -PCLevels)
% Checks if there's any AI in the player configuration Players,
% prompting the user to select its level if an AI player is found
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

% instructions/0
% Predicate displaying the game rules
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
