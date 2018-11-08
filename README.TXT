In order to test the program use swipl and then load the file using
	[pegGame]. 

after being prompted.

In order to test the program use the line:
	play_peg_game.

This will start calling all the necessary functions and start displaying
the results. The program may take several minutes to complete but is
guaranteed to complete. Since there are several solutions only the first
solution for each is printed. The output of run is printed below.

?- play_peg_game.
====Start at 0====
    .
   x x
  x x x
 x x x x
x x x x x
    x
   . x
  . x x
 x x x x
x x x x x
    x
   . x
  x . .
 x x x x
x x x x x
    .
   . .
  x . x
 x x x x
x x x x x
    .
   x .
  . . x
 . x x x
x x x x x
    .
   x x
  . . .
 . x x .
x x x x x
    .
   x x
  . x .
 . . x .
x . x x x
    .
   x x
  . x x
 . . . .
x . . x x
    .
   x .
  . x .
 . . . x
x . . x x
    .
   . .
  . . .
 . . x x
x . . x x
    .
   . .
  . . x
 . . x .
x . . x .
    .
   . .
  . . .
 . . . .
x . x x .
    .
   . .
  . . .
 . . . .
x x . . .
    .
   . .
  . . .
 . . . .
. . x . .
    .
   . .
  . . .
 . . . .
. . x . .
====Start at 1====
    x
   . x
  x x x
 x x x x
x x x x x
    x
   x x
  . x x
 . x x x
x x x x x
    .
   . x
  x x x
 . x x x
x x x x x
    .
   x x
  x . x
 . x . x
x x x x x
    .
   . x
  . . x
 x x . x
x x x x x
    .
   . x
  x . x
 . x . x
. x x x x
    .
   . x
  x x x
 . . . x
. . x x x
    .
   . .
  x . x
 . x . x
. . x x x
    .
   . x
  x . .
 . x . .
. . x x x
    .
   . x
  x . .
 . x . .
. x . . x
    .
   . x
  x x .
 . . . .
. . . . x
    .
   . x
  . . x
 . . . .
. . . . x
    .
   . .
  . . .
 . . . x
. . . . x
    .
   . .
  . . x
 . . . .
. . . . .
    .
   . .
  . . x
 . . . .
. . . . .
====Start at 2====
    x
   x .
  x x x
 x x x x
x x x x x
    x
   x x
  x x .
 x x x .
x x x x x
    .
   x .
  x x x
 x x x .
x x x x x
    .
   x x
  x . x
 x . x .
x x x x x
    .
   x .
  x . .
 x . x x
x x x x x
    .
   x .
  x x .
 x . . x
x x x . x
    .
   . .
  x . .
 x . x x
x x x . x
    .
   . .
  x . .
 x x . .
x x x . x
    .
   x .
  . . .
 . x . .
x x x . x
    .
   x .
  . . .
 . x . .
x . . x x
    .
   x .
  . . .
 . x . .
x . x . .
    .
   x .
  x . .
 . . . .
x . . . .
    .
   . .
  . . .
 x . . .
x . . . .
    .
   . .
  x . .
 . . . .
. . . . .
    .
   . .
  x . .
 . . . .
. . . . .
====Start at 3====
    x
   x x
  . x x
 x x x x
x x x x x
    .
   . x
  x x x
 x x x x
x x x x x
    x
   . .
  x x .
 x x x x
x x x x x
    x
   . .
  . . x
 x x x x
x x x x x
    x
   . x
  . . .
 x x x .
x x x x x
    .
   . .
  . . x
 x x x .
x x x x x
    .
   . .
  x . x
 x . x .
x x . x x
    .
   x .
  . . x
 . . x .
x x . x x
    .
   x .
  . . x
 . . x .
x x x . .
    .
   x .
  . . x
 . . x .
x . . x .
    .
   x .
  . x x
 . . . .
x . . . .
    .
   x .
  x . .
 . . . .
x . . . .
    .
   . .
  . . .
 x . . .
x . . . .
    .
   . .
  x . .
 . . . .
. . . . .
    .
   . .
  x . .
 . . . .
. . . . .
====Start at 4====
    x
   x x
  x . x
 x x x x
x x x x x
    x
   x x
  x x x
 x . x x
x . x x x
    x
   x x
  x x x
 x x . .
x . x x x
    x
   . x
  x . x
 x x x .
x . x x x
    x
   . .
  x . .
 x x x x
x . x x x
    x
   x .
  . . .
 . x x x
x . x x x
    .
   . .
  x . .
 . x x x
x . x x x
    .
   . .
  x . .
 . x x x
x x . . x
    .
   . .
  . . .
 . . x x
x x x . x
    .
   . .
  . . .
 . . x x
x . . x x
    .
   . .
  . . x
 . . x .
x . . x .
    .
   . .
  . . .
 . . . .
x . x x .
    .
   . .
  . . .
 . . . .
x x . . .
    .
   . .
  . . .
 . . . .
. . x . .
    .
   . .
  . . .
 . . . .
. . x . .
true.
