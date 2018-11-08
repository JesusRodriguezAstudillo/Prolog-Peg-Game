% code adapted from https://rosettacode.org/wiki/I.Q._Puzzle
% the database of allowed moves
moves((0,1,3)).
moves((3,1,0)).
moves((0,2,5)).
moves((5,2,0)).
moves((1,3,6)).
moves((6,3,1)).
moves((1,4,8)).
moves((8,4,1)).
moves((2,4,7)).
moves((7,4,2)).
moves((2,5,9)).
moves((9,5,2)).
moves((3,6,10)).
moves((10,6,3)).
moves((3,7,12)).
moves((12,7,3)).
moves((4,7,11)).
moves((11,7,4)).
moves((4,8,13)).
moves((13,8,4)).
moves((5,8,12)).
moves((12,8,5)).
moves((5,9,14)).
moves((14,9,5)).
moves((3,4,5)).
moves((5,4,3)).
moves((6,7,8)).
moves((8,7,6)).
moves((7,8,9)).
moves((9,8,7)).
moves((10,11,12)).
moves((12,11,10)).
moves((11,12,13)).
moves((13,12,11)).
moves((12,13,14)).
moves((14,13,12)).

%find solutions for the first 5 open peg positions
play_peg_game :-
	peg_game(MS0, 0),
	initBoard(MS0, 0),
	peg_game(MS1, 1),
	initBoard(MS1, 1),
	peg_game(MS2, 2),
	initBoard(MS2, 2),
	peg_game(MS3, 3),
	initBoard(MS3, 3),
	peg_game(MS4, 4),
	initBoard(MS4, 4), !.

peg_game(MS, 0) :-
	writeln('====Start at 0===='),
	solve([0], [1,2,3,4,5,6,7,8,9,10,11,12,13,14], [], MS).
peg_game(MS, 1) :-
	writeln('====Start at 1===='),
	solve([1], [0,2,3,4,5,6,7,8,9,10,11,12,13,14], [], MS).
peg_game(MS, 2) :-
	writeln('====Start at 2===='),
	solve([2], [1,0,3,4,5,6,7,8,9,10,11,12,13,14], [], MS).
peg_game(MS, 3) :-
	writeln('====Start at 3===='),
	solve([3], [1,2,0,4,5,6,7,8,9,10,11,12,13,14], [], MS).
peg_game(MS, 4) :-
	writeln('====Start at 4===='),
	solve([4], [1,2,3,0,5,6,7,8,9,10,11,12,13,14], [], MS).

solve(_, [_], L, MS) :-
	reverse(L, MS).
solve(Opened, Used, L, MS) :-
	select(F, Used, Rest), %select a peg from the occupied set
	select(O, Rest, Remaining), %select another occupied set
	select(T, Opened, RestOpened), %select an open peg
	moves((T,O,F)), %check if the move is valid
	solve([F, O|RestOpened], [T|Remaining], [(T,O,F) | L], MS). %append the vairables and find the next move

%code for the function below was taken from:
%https://stackoverflow.com/questions/41454755/how-can-i-replace-an-element-of-a-list-using-an-index-in-prolog
%all credit goes to the author
changeAt([_|T],0,E,[E|T]).
changeAt([H|T],P,E,[H|R]) :-
    P > 0, NP is P-1, changeAt(T,NP,E,R).

% initialize the board 
initBoard(MS, 0) :- 
	findall('x',between(0,14,_),Board),
	changeAt(Board, 0, ., InitBoard),
	[IB0,IB1,IB2,IB3,IB4,IB5,IB6,IB7,IB8,IB9,IB10,IB11,IB12,IB13,IB14]=InitBoard,
	format('    ~w ~n', [IB0]),
	format('   ~w ~w ~n', [IB1,IB2]),
	format('  ~w ~w ~w ~n', [IB3,IB4,IB5]),
	format(' ~w ~w ~w ~w ~n', [IB6,IB7,IB8,IB9]),
	format('~w ~w ~w ~w ~w~n', [IB10,IB11,IB12,IB13,IB14]),
	printBoard(MS,Board).

initBoard(MS, 1) :- 
	findall('x',between(0,14,_),Board),
	changeAt(Board, 1, ., InitBoard),
	[IB0,IB1,IB2,IB3,IB4,IB5,IB6,IB7,IB8,IB9,IB10,IB11,IB12,IB13,IB14]=InitBoard,
	format('    ~w ~n', [IB0]),
	format('   ~w ~w ~n', [IB1,IB2]),
	format('  ~w ~w ~w ~n', [IB3,IB4,IB5]),
	format(' ~w ~w ~w ~w ~n', [IB6,IB7,IB8,IB9]),
	format('~w ~w ~w ~w ~w~n', [IB10,IB11,IB12,IB13,IB14]),
	printBoard(MS,Board).

initBoard(MS, 2) :- 
	findall('x',between(0,14,_),Board),
	changeAt(Board, 2, ., InitBoard),
	[IB0,IB1,IB2,IB3,IB4,IB5,IB6,IB7,IB8,IB9,IB10,IB11,IB12,IB13,IB14]=InitBoard,
	format('    ~w ~n', [IB0]),
	format('   ~w ~w ~n', [IB1,IB2]),
	format('  ~w ~w ~w ~n', [IB3,IB4,IB5]),
	format(' ~w ~w ~w ~w ~n', [IB6,IB7,IB8,IB9]),
	format('~w ~w ~w ~w ~w~n', [IB10,IB11,IB12,IB13,IB14]),
	printBoard(MS,Board).

initBoard(MS, 3) :- 
	findall('x',between(0,14,_),Board),
	changeAt(Board, 3, ., InitBoard),
	[IB0,IB1,IB2,IB3,IB4,IB5,IB6,IB7,IB8,IB9,IB10,IB11,IB12,IB13,IB14]=InitBoard,
	format('    ~w ~n', [IB0]),
	format('   ~w ~w ~n', [IB1,IB2]),
	format('  ~w ~w ~w ~n', [IB3,IB4,IB5]),
	format(' ~w ~w ~w ~w ~n', [IB6,IB7,IB8,IB9]),
	format('~w ~w ~w ~w ~w~n', [IB10,IB11,IB12,IB13,IB14]),
	printBoard(MS,Board).

initBoard(MS, 4) :- 
	findall('x',between(0,14,_),Board),
	changeAt(Board, 4, ., InitBoard),
	[IB0,IB1,IB2,IB3,IB4,IB5,IB6,IB7,IB8,IB9,IB10,IB11,IB12,IB13,IB14]=InitBoard,
	format('    ~w ~n', [IB0]),
	format('   ~w ~w ~n', [IB1,IB2]),
	format('  ~w ~w ~w ~n', [IB3,IB4,IB5]),
	format(' ~w ~w ~w ~w ~n', [IB6,IB7,IB8,IB9]),
	format('~w ~w ~w ~w ~w~n', [IB10,IB11,IB12,IB13,IB14]),
	printBoard(MS,Board).

printBoard(MS, Board) :-
	select(X,MS,Rest), (A,B,C)=X,
	changeAt(Board, A, 'x', NewBoard),
	changeAt(NewBoard, B, '.', NewBoard2),
	changeAt(NewBoard2, C, '.', NewBoard3),
	[IB0,IB1,IB2,IB3,IB4,IB5,IB6,IB7,IB8,IB9,IB10,IB11,IB12,IB13,IB14]=NewBoard3,
	format('    ~w ~n', [IB0]),
	format('   ~w ~w ~n', [IB1,IB2]),
	format('  ~w ~w ~w ~n', [IB3,IB4,IB5]),
	format(' ~w ~w ~w ~w ~n', [IB6,IB7,IB8,IB9]),
	format('~w ~w ~w ~w ~w~n', [IB10,IB11,IB12,IB13,IB14]),
	printBoard(Rest, NewBoard3).

printBoard([],Board) :-
	[IB0,IB1,IB2,IB3,IB4,IB5,IB6,IB7,IB8,IB9,IB10,IB11,IB12,IB13,IB14]=Board,
	format('    ~w ~n', [IB0]),
	format('   ~w ~w ~n', [IB1,IB2]),
	format('  ~w ~w ~w ~n', [IB3,IB4,IB5]),
	format(' ~w ~w ~w ~w ~n', [IB6,IB7,IB8,IB9]),
	format('~w ~w ~w ~w ~w~n', [IB10,IB11,IB12,IB13,IB14]).
