% load common predicates
:- ['common.pl'].
:- consult('common.pl').

% to reset facts, on each knowledge base load.
:- abolish(best_run/1).
:- abolish(best_path/1).

best_run(13). % shortest path (to the nearest non-covid object) max length.
best_path([]). % to store answer

% to indicate that it will change dynamically.
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

% backtracking routine
go(StepCount, [H|T], NextMove, Protected) :-
    best_run(B),
    StepCount < B,

    % NextMove was legal, actor is in now in some location X after the move.
    move(H, NextMove, location(Ax, Ay), Protected), 

    % that new location was not visited before.
    \+ memberchk(location(Ax, Ay), T), % Optimization: can you check visited in a faster way?
    
    % append the new location to the path.
    append([location(Ax, Ay), H], T, Path), % Optimization: can you reduce (U, L) to UL? 
    
    % is the new location a protection zone?
    (protection(location(Ax, Ay)) -> P is 1 ; P is Protected),

    Sp1 is StepCount + 1, % now our move can lead to a solution, try going further
    l(L), r(R), u(U), d(D), ul(UL), ur(UR), bl(BL), br(BR), % get aliases defined above
  
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


% Hard-coded (impossible) map for custom testing
% TO USE: uncomment the following lines and comment the first line 'get_random_map' in predicate 'start_backtrack'

% home(location(1, 7)).
% covid(location(1, 5)).
% covid(location(3, 7)).
% protection(location(0, 7)).
% protection(location(0, 8)).

start_backtrack :-
    get_random_map,
    write("Please allow up to 1 minute, backtracking is not the best algorithm for shortest path problems!"), nl,
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
    \+ start_backtrack -> test_util; true.

test_bt :-
    ['backtracking.pl'],
    time(test_util).
