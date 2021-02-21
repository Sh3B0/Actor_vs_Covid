% to reset facts, on each knowledge base load.
:- abolish(home/1).
:- abolish(covid/1).
:- abolish(covid_zone/1).
:- abolish(protection/1).
:- abolish(best_run/1).

best_run(100). % shortest path length, 100 if no path was found (no valid path can exceed 100 in 9*9 lattice).
:- dynamic(best_run/1). % to indicate that it will change dynamically.

% checks if a location is valid. 
location(X, Y) :-
    between(0, 8, X),
    between(0, 8, Y).

% hard-coding a sample map, to be replaced by map generator later.
% mask and doctor are the same effictively, thus we can denote them as "protection".
/*
    .........
    .H....C..
    .........
    .........
    .C..D....
    .........
    ........
    .......M.
    A........
*/

home(location(8, 8)).
covid(location(7, 3)).
covid(location(7, 6)).
protection(location(0, 7)). 
protection(location(0, 8)).

% checks if a location is a covid zone.
covid_zone(location(X, Y)) :-
    covid(location(A, B)),
    (
        (X =:= A, Y =:= B + 1);
        (X =:= A, Y =:= B - 1);
        (X =:= A + 1, Y =:= B);
        (X =:= A + 1, Y =:= B + 1);
        (X =:= A + 1, Y =:= B - 1);
        (X =:= A - 1, Y =:= B);
        (X =:= A - 1, Y =:= B + 1);
        (X =:= A - 1, Y =:= B - 1)
    ).

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

% checks if a delta move is valid.
delta(Dx, Dy) :-
    \+ (Dx == 0, Dy == 0),
    between(-1, 1, Dx),
    between(-1, 1, Dy).

% directions
l(delta(0, -1)).
r(delta(0, 1)).
u(delta(-1, 0)).
d(delta(1, 0)).
ul(delta(-1, -1)).
ur(delta(-1, 1)).
bl(delta(1, -1)).
br(delta(1, 1)).


go(StepCount, [H|T], NextMove, Protected) :-
    best_run(B),
    StepCount < B,

    % NextMove was legal, actor is in now in some location X after the move.
    move(H, NextMove, location(Ax, Ay), Protected), 

    % Opt: can you check visited in a faster way?
    \+ member(location(Ax, Ay), T), % that new location was not visited before.
    
    % Opt: can you reduce (U, L) to UL?
    append([location(Ax, Ay), H], T, Path), % append the new location to the path.
    
    % is the new location a protection zone?
    (protection(location(Ax, Ay)) -> P is 1 ; P is Protected),

    Sp1 is StepCount + 1, % now our move can lead to a solution, try going further
    l(L), r(R), u(U), d(D), ul(UL), ur(UR), bl(BL), br(BR),

    % Opt: can you make a guided search, try the calls that are more likely to get you home first.        
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
        % (Hx = Ax, Hy > Ay ->
        %     (
        %         go(Sp1, Path, R, P);
        %         go(Sp1, Path, BR, P);
        %         go(Sp1, Path, UR, P);
        %         go(Sp1, Path, D, P);
        %         go(Sp1, Path, U, P);
        %         go(Sp1, Path, UL, P);
        %         go(Sp1, Path, BL, P);
        %         go(Sp1, Path, L, P)
        %     ) ; true
        % ),
        % (Hx = Ax, Hy < Ay ->
        %     (
        %         go(Sp1, Path, L, P);
        %         go(Sp1, Path, BL, P);
        %         go(Sp1, Path, UL, P);
        %         go(Sp1, Path, D, P);
        %         go(Sp1, Path, U, P);
        %         go(Sp1, Path, UR, P);
        %         go(Sp1, Path, BR, P);
        %         go(Sp1, Path, R, P)
        %     ) ; true
        % ),
        % (Hx < Ax, Hy = Ay ->
        %     (
        %         go(Sp1, Path, U, P);
        %         go(Sp1, Path, UL, P);
        %         go(Sp1, Path, UR, P);
        %         go(Sp1, Path, L, P);
        %         go(Sp1, Path, R, P);
        %         go(Sp1, Path, BL, P);
        %         go(Sp1, Path, BR, P);
        %         go(Sp1, Path, D, P)
        %     ) ; true
        % ),
        % (Hx > Ax, Hy = Ay ->
        %     (
        %         go(Sp1, Path, D, P);
        %         go(Sp1, Path, BL, P);
        %         go(Sp1, Path, BR, P);
        %         go(Sp1, Path, L, P);
        %         go(Sp1, Path, R, P);
        %         go(Sp1, Path, UL, P);
        %         go(Sp1, Path, UR, P);
        %         go(Sp1, Path, U, P)
        %     ) ; true
        % )
    ).
        

% base case: maximize score and return if reached home.
go(StepCount, [H|T], _, _) :-
    home(H),
    best_run(X),
    StepCount < X,
    write([H|T]), nl,
    write(StepCount), nl,
    assert(best_run(StepCount)),
    retract(best_run(X)).
    
start :-
    (go(0, [location(8, 0)], delta(0, 1), 0);
    go(0, [location(8, 0)], delta(-1, 0), 0);
    go(0, [location(8, 0)], delta(-1, 1), 0)),
    best_run(X),
    write(X), nl.
