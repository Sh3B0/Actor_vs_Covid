:- abolish(edge/2).
:- dynamic(edge/2).

start :-
    location(A, B), location(C, D),
    (
        (A =:= C, B =:= D+1);
        (A =:= C, B =:= D-1);
        (A =:= C+1, B =:= D+1);
        (A =:= C+1, B =:= D-1);
        (A =:= C-1, B =:= D+1);
        (A =:= C-1, B =:= D-1)
    ),
    assert(edge(location(A, B), location(C, D))).

% edge(X, Y, 1) :-
%     move(X, _, Y, _).


location(X, Y) :-
    between(0, 8, X),
    between(0, 8, Y).
