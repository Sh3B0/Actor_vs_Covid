:- ['common.pl'].
:- consult('common.pl').

:- abolish(dist/1).
:- dynamic(dist/2).
:- dynamic(s/2).

start :-
    location(A, B), location(C, D),
    distance(location(A, B), location(C, D), 1), \+ covid_zone(location(A, B)), \+ covid_zone(location(C, D)),
    assert(s(location(A, B), location(C, D))).

goal(location(8, 6)).

solve(Start, Solution) :-
    breadthfirst([ [Start] ], Solution).

breadthfirst([ [Node | Path] |_], [Node | Path] ) :-
    goal(Node).

breadthfirst([ [N | Path] | Paths], Solution) :-
    bagof([M,N|Path],
    (s( N, M), \+ member( M, [N | Path] ) ), NewPaths),
    append(Paths, NewPaths, Pathsl), !,
    breadthfirst(Pathsl, Solution);
    breadthfirst(Paths, Solution).
