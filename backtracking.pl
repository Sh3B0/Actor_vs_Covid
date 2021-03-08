% load common file for predicates, facts and rules used by both algoruthms
:- ['common.pl'].

% to reset facts, on each knowledge base load.
:- abolish(best_run/1).
:- abolish(best_path/1).

best_run(13). % shortest path (to the nearest non-covid object) max length + 1.
best_path([]). % to store final answer

% to indicate facts that will change dynamically.
:- dynamic(best_run/1). 
:- dynamic(best_path/1).

/*
    Actor move rule: succeeds if moved to a valid location and the actor is safe from covid.
        move(A, D, B, P): moves actor from point A in direction D to reach point B
        P indicates whether the actor is protected or not.
*/
move(location(X, Y), delta(Dx, Dy), location(Xn, Yn), P) :-
    X_2 is X + Dx,
    Y_2 is Y + Dy,
    location(X_2, Y_2),
    (
        \+ covid_zone(location(X_2, Y_2));
        P =:= 1
    ),
    Xn is X_2,
    Yn is Y_2.

% checks if a delta vector is valid.
delta(Dx, Dy) :-
    \+ (Dx == 0, Dy == 0),
    between(-1, 1, Dx),
    between(-1, 1, Dy).

% direction aliases to make my life easier
l(delta(0, -1)).
r(delta(0, 1)).
u(delta(-1, 0)).
d(delta(1, 0)).
ul(delta(-1, -1)).
ur(delta(-1, 1)).
bl(delta(1, -1)).
br(delta(1, 1)).

% backtracking routine, second parameter stores the path till the moment.
go(StepCount, [H|T], NextMove, Protected) :-

    % Optimization: breaks if there was a path discovered before that is shorter than current.
    best_run(B),
    StepCount < B,

    % NextMove was legal, actor is now in some location(Ax, Ay) after the move.
    move(H, NextMove, location(Ax, Ay), Protected), 

    % that new location was not visited before.
    \+ memberchk(location(Ax, Ay), T),
    
    % append the new location to the path.
    append([location(Ax, Ay), H], T, Path),
    
    % is the new location a protection zone? if so, mark the actor as protected.
    (protection(location(Ax, Ay)) -> P is 1 ; P is Protected),

    Sp1 is StepCount + 1, % now our move can lead to a solution, try going further
    l(L), r(R), u(U), d(D), ul(UL), ur(UR), bl(BL), br(BR), % get delta aliases defined above
  
    % recursive backtracking calls, optimized to check the paths that are more likely to get actor home faster first.
    home(location(Hx, Hy)),    
    (
        (Hx =< Ax, Hy >= Ay ->
            (
                go(Sp1, Path, UR, P);
                go(Sp1, Path, U, P);
                go(Sp1, Path, R, P);
                go(Sp1, Path, UL, P);
                go(Sp1, Path, BR, P);
                go(Sp1, Path, D, P);
                go(Sp1, Path, L, P);
                go(Sp1, Path, BL, P)
            ) ; true
        ),
        (Hx >= Ax, Hy =< Ay ->
            (
                go(Sp1, Path, BL, P);
                go(Sp1, Path, D, P);
                go(Sp1, Path, L, P);
                go(Sp1, Path, BR, P);
                go(Sp1, Path, UL, P);
                go(Sp1, Path, U, P);
                go(Sp1, Path, R, P);
                go(Sp1, Path, UR, P)
            ) ; true
        ),
        (Hx =< Ax, Hy =< Ay ->
            (
                go(Sp1, Path, UL, P);
                go(Sp1, Path, U, P);
                go(Sp1, Path, L, P);
                go(Sp1, Path, UR, P);
                go(Sp1, Path, BL, P);
                go(Sp1, Path, D, P);
                go(Sp1, Path, R, P);
                go(Sp1, Path, BR, P)
            ) ; true
        ),
        (Hx >= Ax, Hy >= Ay ->
            (
                go(Sp1, Path, BR, P);
                go(Sp1, Path, D, P);
                go(Sp1, Path, R, P);
                go(Sp1, Path, BL, P);
                go(Sp1, Path, UR, P);
                go(Sp1, Path, U, P);
                go(Sp1, Path, L, P);
                go(Sp1, Path, UL, P)
            ) ; true
        )
    ).
        
% base case: maximize score and return if reached home.
go(StepCount, [CurrentLocation|T], _, _) :-
    (
        home(CurrentLocation),
        best_run(B),
        StepCount < B,
        %write(B), nl,
        assert(best_run(StepCount)),
        retract(best_run(B)),
        best_path(BP),
        assert(best_path([CurrentLocation|T])),
        retract(best_path(BP))
    ).

% backtracking initial calls
backtrack :-
    (
        go(0, [location(8, 0)], delta(0, 1), 0);
        go(0, [location(8, 0)], delta(-1, 0), 0);
        go(0, [location(8, 0)], delta(-1, 1), 0)
    ).

% applies algorithm, and writes results
start_backtrack :-
    write("Please allow up to 2 minutes, backtracking is not the best algorithm for shortest path problems!"), nl,
    (backtrack -> true; true),
    best_run(X),
    best_path(P),
    F is 0,
    (P = [] -> (write("No path was found"), F is 1); true),
    F =:= 0 ->
    (
        write('Shortest path length: '),
        write(X), nl, 
        write('Shortest path: '),
        reverse(P, Pr),
        write(Pr), nl,
        assert(best_run(12)), !
    ); true.

% retry if test failed
test_util :-
    \+ start_backtrack -> test_util; true.

% starting point (map has to be defined before usage, check main.pl)
test_bt :-
    time(test_util).
