% initial_board(+RowNumber, -Row)
initial_board(0, [blank, blank, tent1, head1, head1, tent1, blank, blank]).
initial_board(1, [blank, blank, tent1, head1, head1, tent1, blank, blank]).
initial_board(2, [blank, blank, tent1, tent1, tent1, tent1, blank, blank]).
initial_board(3, [blank, blank, blank, blank, blank, blank, blank, blank]).
initial_board(4, [blank, blank, blank, blank, blank, blank, blank, blank]).
initial_board(5, [blank, blank, tent2, tent2, tent2, tent2, blank, blank]).
initial_board(6, [blank, blank, tent2, head2, head2, tent2, blank, blank]).
initial_board(7, [blank, blank, tent2, head2, head2, tent2, blank, blank]).

% menu_option(+OptionNumber, -Option)
menu_option(1, play).
menu_option(2, instructions).
menu_option(3, quit).

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