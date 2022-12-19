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


valid_moves(GameState, Player, ListOfMoves) :-
    findall(Move, move(GameState, Move, NewState), ListOfMoves).

choose_move(GameState, Player, Level, Move) :-
    true.

value(GameState, Player, Value).
  
game_cycle(GameState-Player) :-
    game_over(GameState, Winner), !,
    congratulate(Winner).

game_cycle(GameState-Player) :-
    choose_move(GameState, Player, Move),
    move(GameState, Move, NewGameState),
    next_player(Player, NextPlayer),
    display_game(GameState-NextPlayer), !,
    game_cycle(NewGameState-NextPlayer).

play :-
    initial_state(GameState-Player),
    display_game(GameState-Player).
    %game_cycle(GameState-Player).

