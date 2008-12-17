This is an implementation of the RIT CS4 puzzle solver project.

History
=======

The RIT CS4 course is (or, at least, was) the first C++ course taught
to Computer Science students at RIT (generally in the second year).  The
most common project used for this course is a puzzle solver project,
which requires around 4 solutions over the 10-week period.

I've TA'ed for this course many times, and I've accumulated various
solutions.  I've placed a few up here, in the hopes that they may server
as useful examples for other people who've *already taken* the course.

If I discover that the C++ version is being abused, I'll take it down.

Project
=======

(The most up-to-date version of this project can be found here:
[CS4 Projects page](http://www.cs.rit.edu/~cs4/Projects/)).

In general, the project is split into two parts:

 1. A generalized solver : the primary piece of the project is a solver
    that will perform a breadth-first search of a given problem space.
    It can solve puzzles that provide the following:
    * A starting position
    * A function for getting "next" or "neighbor" positions
    * A function for determining if a given position is the "end"
 2. Various puzzles : Puzzles implement the required functionally plus
    a minimal `main` method for starting from the command-line.

The general flow of the program is:
 1. The puzzle's `main` method takes care of configuring the puzzle as
    specified (either as arguments to the command line or as names of
    files specified on the command line - the project is also used to
    teach the basics of file/stream input/output in C++).
 2. The puzzle's `main` method starts up the solver
 3. The solver searches over the problem space until it finds a solution
    or runs out of unique and new configurations
 4. The solver prints out the path (*a* shortest path) to the solution.

The project is always taught in C++.

Common puzzles
=============

Here are a few of the commonly-used puzzles:

Clock
-----

This one is rather simple - you have a clock (generally, a 12-hour clock,
but the number of hours can be specified), and you want to find the
shortest "path" the hour hand can take to get from one hour to the next.
The clock, of course, "wraps around" at the largest hour to the smallest.

*Example:*

> A clock that has `12` hours on it.
> The current hour is `3`, the desired hour is `6`.
> The shortest path is: `3, 4, 5, 6`.

VClock (Variable Clock)
-----------------------

This is very similar to `Clock`, except that the hand can step in increments
of more than just +1/-1 (they are specified on the command line).

*Example:*

> A "vclock" that has `12` hours on it.
> The current hour is `2`, the desired hour is `9`.
> The possible steps are `+2`, `+1`, and `-3`.
> The shortest path is `2 (-3) 11 (-3) 8 (+1) 9`.

Water
-----

This is the "water bucket" puzzle that most people are familiar with - you
have multiple buckets or containers of water, each which can hold a specific
amount.  By filling buckets up with water, pouring them into each other, and
emptying buckets, your goal is to make one bucket contain the desired about
of water (measured in hand-wavy "units").

The most common example is that you have two containers that hold `3` and `5`
units, respectively.  The goal amount is `4` units of water.

The shortest path is:
> (0, 0)
> (0, 3)
> (3, 3)
> (1, 5)
> (1, 0)
> (0, 1)
> (3, 1)
> (0, 4)

(TODO: More examples to come)

Implementations
===============

C++
---

Most of these puzzles have implementations in C++ that I wrote during the
courses.  As there are 3 puzzles taught each time, and the puzzles change
up (some are there almost every time, like `clock`), I have a few that
I've written.  I don't remember which 3 puzzles were assigned the year I
*took* the course.

Lisp
----

The summer before my senior year, while on co-op for Microsoft, I wrote
a version of the puzzle solver and a few of the puzzles in Lisp.  I had
two goals for this:

 1. To learn more about Lisp - I love the language, but I wanted to actually
    *use* it :)
 2. To see if Lisp really is seventy-gigabazillion-times slower than C++ -
    I heard this one a lot, mostly from people who suck at programming in
    both languages, so I wanted to see how true it was.

I also wrote it around the time I read *Practical Common Lisp*, so I included
a tiny unit testing framework from that book.

I learned a few things from this:

 * It is **amazing** how easy it was to add unit testing.  Even writing
   Java in the Eclipse environment (with JUnit) is still more hassle than
   what I did here.  The upside (in Eclipse/JUnit) are the pretty red and
   green bars, but I found I could live without those :)
 * Related to the above bullet, I wrote a very small amount of code, and
   almost no code that wasn't directly related to exactly what I needed
   to do.  At some level, this means very little of the overhead of
   fairly useless keywords (e.g., "how many times do I have to declare that
   this variable is an int?).  At a more important level, it means that my
   code is very *semantically compressed* - I only have to write that which
   will bring me closer to my goal, and not 7,000 pounds of "public static
   void main blah blah blah open-bracket close-bracket".
 * As for the speed - the lisp solution (running on a few different lisps on
   Ubuntu at the time), after compiled (using 
   `(load (compile-file "foo.lisp"))`), was *just as fast* as the C++ solution
   compiled (without much or any optimization, though).  Compiling the C++
   stuff with full optimization made it faster (though much bigger), but I'd
   still trade the milliseconds for the many other positives of Lisp.

Clojure
-------

This one I've just decided to do.  Nothing here yet, but soon :)

