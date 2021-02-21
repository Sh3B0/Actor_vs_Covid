:- ['map.pl'].
:- consult('map.pl').

% to reset facts, on each knowledge base load.
:- abolish(home/1).
:- abolish(covid/1).
:- abolish(covid_zone/1).
:- abolish(protection/1).
:- abolish(best_run/1).
:- abolish(best_path/1).

best_run(13). % shortest path length, 17 if no path was found (It can be proved that no valid path can exceed 16 in 9*9 lattice).
best_path([]).

:- dynamic(best_run/1). % to indicate that it will change dynamically.
:- dynamic(best_path/1).
:- dynamic(home/1).
:- dynamic(covid/1).
:- dynamic(protection/1).

% checks if a location is valid. 
location(X, Y) :-
    between(0, 8, X),
    between(0, 8, Y).

% hard-coding a sample map, to be replaced by map generator later.
% mask and doctor are the same effictively, thus we can denote them as "protection".
get_random_map :-
    gen_map,
    home_m(H),
    covid1(C1),
    covid2(C2),
    protection1(P1),
    protection2(P2),
    
    assert(covid(C1)),
    assert(covid(C2)),

    ((covid_zone(H); covid_zone(P1); covid_zone(P2); covid_zone(location(8, 0))) -> throw('Invalid map') ; true),

    assert(home(H)),
    assert(protection(P1)),
    assert(protection(P2)),

    write('Generated map:'), nl,
    v(0, 0),v(0, 1),v(0, 2),v(0, 3),v(0, 4),v(0, 5),v(0, 6),v(0, 7),v(0, 8),
    v(1, 0),v(1, 1),v(1, 2),v(1, 3),v(1, 4),v(1, 5),v(1, 6),v(1, 7),v(1, 8),
    v(2, 0),v(2, 1),v(2, 2),v(2, 3),v(2, 4),v(2, 5),v(2, 6),v(2, 7),v(2, 8),
    v(3, 0),v(3, 1),v(3, 2),v(3, 3),v(3, 4),v(3, 5),v(3, 6),v(3, 7),v(3, 8),
    v(4, 0),v(4, 1),v(4, 2),v(4, 3),v(4, 4),v(4, 5),v(4, 6),v(4, 7),v(4, 8),
    v(5, 0),v(5, 1),v(5, 2),v(5, 3),v(5, 4),v(5, 5),v(5, 6),v(5, 7),v(5, 8),
    v(6, 0),v(6, 1),v(6, 2),v(6, 3),v(6, 4),v(6, 5),v(6, 6),v(6, 7),v(6, 8),
    v(7, 0),v(7, 1),v(7, 2),v(7, 3),v(7, 4),v(7, 5),v(7, 6),v(7, 7),v(7, 8),
    v(8, 0),v(8, 1),v(8, 2),v(8, 3),v(8, 4),v(8, 5),v(8, 6),v(8, 7),v(8, 8),
    write("Please allow up to 1 minute, backtracking is not the best algorithm for shortest path problems!"), nl.

v(X, Y) :-
    (
        (
            (home(location(X, Y)) -> write('H') ; false);
            (covid(location(X, Y)) -> write('C') ; false);
            (protection(location(X, Y)) -> write('P') ; false);
            (X = 8, Y = 0 -> write('A') ; false)
        );
        write('.')
    ),
    (Y = 8 -> nl; true).
    
% checks if a location is a covid zone.
% Opt: save results somewhere so you don't compute them multiple times.
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
    \+ memberchk(location(Ax, Ay), T), % that new location was not visited before.
    
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
    ).
        

% base case: maximize score and return if reached home.
go(StepCount, [CurrentLocation|T], _, _) :-
    (
        home(CurrentLocation),
        best_run(X),
        StepCount < X,
        %write(X), nl,
        assert(best_run(StepCount)),
        retract(best_run(X)),
        best_path(BP),
        reverse([CurrentLocation|T], BPR),
        assert(best_path(BPR)),
        retract(best_path(BP))
    ).
    % ;(
    %     protection(CurrentLocation),
    %     home(HomeLocation),
    %     distance(CurrentLocation, HomeLocation, SC),
    %     SCN is StepCount + SC,
    %     best_run(X),
    %     assert(best_run(SCN)),
    %     best_path(BP),
    %     gen_path(CurrentLocation, HomeLocation, Tmp),
    %     write("TMP="),
    %     write(tmp),nl,
    %     append(Tmp, BP, New),
    %     reverse(New, Result),
    %     assert(best_path(Result)),
    %     retract(best_path(BP))
    % ).

% distance(location(A, B), location(C, D), Result) :-
%     Result = max(abs(A-C), abs(B-D)).

% gen_path(location(A, B), location(C, D), Result) :-
%     Ap1 is A+1, Am1 is A-1, Bp1 is B+1, Bm1 is B-1,
%     (A=B, C=D -> true; true),
%     (A<C, B<D -> gen_path(location(Ap1, Bp1), location(C, D), [location(Ap1, Bp1)|Result])),
%     (A>C, B>D -> gen_path(location(Am1, Bm1), location(C, D), [location(Am1, Bm1)|Result])),
%     (A<C, B>D -> gen_path(location(Ap1, Bm1), location(C, D), [location(Ap1, Bm1)|Result])),
%     (A>C, B<D -> gen_path(location(Am1, Bp1), location(C, D), [location(Am1, Bp1)|Result])),
%     (A=C, B<D -> gen_path(location(A, Bp1), location(C, D), [location(A, Bp1)|Result])),
%     (A=C, B>D -> gen_path(location(A, Bm1), location(C, D), [location(A, Bm1)|Result])),
%     (A<C, B=D -> gen_path(location(Ap1, B), location(C, D), [location(Ap1, B)|Result])),
%     (A>C, B=D -> gen_path(location(Am1, B), location(C, D), [location(Am1, B)|Result])).

backtrack :-
    (
        go(0, [location(8, 0)], delta(0, 1), 0);
        go(0, [location(8, 0)], delta(-1, 0), 0);
        go(0, [location(8, 0)], delta(-1, 1), 0)
    ).
    % (
    %     protection(P),
    %     assert(home(P)),
    %     (
    %         go(0, [location(8, 0)], delta(0, 1), 0);
    %         go(0, [location(8, 0)], delta(-1, 0), 0);
    %         go(0, [location(8, 0)], delta(-1, 1), 0)
    %     )
    % )


% % hard-coded map for testing
% home(location(1, 7)).
% covid(location(6, 0)).
% covid(location(8, 2)).
% protection(location(0, 7)).
% protection(location(0, 8)).

start :-
    get_random_map,
    (backtrack -> true; true),
    best_run(X),
    best_path(P),
    (P = [] -> (throw("No path was found")); true),
    write('Shortest path length: '),
    write(X), nl, 
    write('Shortest path: '),
    write(P), nl,
    assert(best_run(12)), !.

test_util :-
    \+ start -> test_util; true.

test :-
    time(test_util).