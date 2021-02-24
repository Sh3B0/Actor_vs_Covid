% Common file for usage by the two algorithms

% to reset facts, on each knowledge base load.
:- abolish(home_m/1).
:- abolish(covid1/1).
:- abolish(covid2/1).
:- abolish(protection1/1).
:- abolish(protection2/1).
:- abolish(home/1).
:- abolish(covid/1).
:- abolish(protection/1).

% to indicate facts that will change dynamically.
:- dynamic(home/1).
:- dynamic(covid/1).
:- dynamic(protection/1).
:- dynamic(home_m/1).
:- dynamic(covid1/1).
:- dynamic(covid2/1).
:- dynamic(protection1/1).
:- dynamic(protection2/1).

actor(location(8, 0)). % actor initial location.

% generates a valid (non-trivial) map
gen_map :-
    random_between(0, 5, Hx),
    random_between(3, 8, Hy), 
    random_between(0, 8, C1x),
    random_between(0, 8, C1y), 
    random_between(0, 8, C2x),
    random_between(0, 8, C2y),
    random_between(0, 8, P1x),
    random_between(0, 8, P1y),
    random_between(0, 8, P2x),
    random_between(0, 8, P2y),
    
    assert(home_m(location(Hx, Hy))),
    assert(covid1(location(C1x, C1y))),
    assert(covid2(location(C2x, C2y))),
    assert(protection1(location(P1x, P1y))),
    assert(protection2(location(P2x, P2y))),
    actor(A), home_m(H), covid1(C1), covid2(C2), protection1(P1), protection2(P2),
    % validates the map
    (
        (
            H = C1; H = C2; H = P1; H = P2; C1 = C2; C1 = P1; C1 = P2; C2 = P1; C2 = P2; P1 = P2; H = A; P1 = A; P2 = A; C1 = A; C2 = A; 
            distance(H, C1, 1); distance(H, C2, 1); distance(P1, C1, 1); distance(P2, C1, 1); distance(P1, C2, 1); distance(P2, C2, 1);
            distance(location(8, 0), C1, 1); distance(location(8, 0), C2, 1) 
        ) -> (retract(home_m(H)), retract(covid1(C1)), retract(covid2(C2)), retract(protection1(P1)), retract(protection2(P2)), gen_map) ; true
    ).

% invokes the generate map rule, and stores the results as dynamic facts
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

    write('Generated map:'), nl, % sorry but this was faster to write :D
    v(0, 0),v(0, 1),v(0, 2),v(0, 3),v(0, 4),v(0, 5),v(0, 6),v(0, 7),v(0, 8),
    v(1, 0),v(1, 1),v(1, 2),v(1, 3),v(1, 4),v(1, 5),v(1, 6),v(1, 7),v(1, 8),
    v(2, 0),v(2, 1),v(2, 2),v(2, 3),v(2, 4),v(2, 5),v(2, 6),v(2, 7),v(2, 8),
    v(3, 0),v(3, 1),v(3, 2),v(3, 3),v(3, 4),v(3, 5),v(3, 6),v(3, 7),v(3, 8),
    v(4, 0),v(4, 1),v(4, 2),v(4, 3),v(4, 4),v(4, 5),v(4, 6),v(4, 7),v(4, 8),
    v(5, 0),v(5, 1),v(5, 2),v(5, 3),v(5, 4),v(5, 5),v(5, 6),v(5, 7),v(5, 8),
    v(6, 0),v(6, 1),v(6, 2),v(6, 3),v(6, 4),v(6, 5),v(6, 6),v(6, 7),v(6, 8),
    v(7, 0),v(7, 1),v(7, 2),v(7, 3),v(7, 4),v(7, 5),v(7, 6),v(7, 7),v(7, 8),
    v(8, 0),v(8, 1),v(8, 2),v(8, 3),v(8, 4),v(8, 5),v(8, 6),v(8, 7),v(8, 8).
    
% visualize the element at location(X, Y)
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

% appends a direct (reversed) path from current location(A, B) (list head) to home
% used after getting protection to construct a path home directly. 
gen_path([location(A, B)|T], Result) :-
    home(location(C, D)),
    (
        (A=:=C, B=:=D -> Result = [location(A, B)|T]; false);
        (A<C, B<D -> Ap1 is A+1, Bp1 is B+1, gen_path([location(Ap1, Bp1), location(A, B)|T], Result));
        (A>C, B>D -> Am1 is A-1, Bm1 is B-1, gen_path([location(Am1, Bm1), location(A, B)|T], Result));
        (A<C, B>D -> Ap1 is A+1, Bm1 is B-1, gen_path([location(Ap1, Bm1), location(A, B)|T], Result));
        (A>C, B<D -> Am1 is A-1, Bp1 is B+1, gen_path([location(Am1, Bp1), location(A, B)|T], Result));
        (A=:=C, B<D -> Bp1 is B+1, gen_path([location(A, Bp1), location(A, B)|T], Result));
        (A=:=C, B>D -> Bm1 is B-1, gen_path([location(A, Bm1), location(A, B)|T], Result));
        (A<C, B=:=D -> Ap1 is A+1, gen_path([location(Ap1, B), location(A, B)|T], Result));
        (A>C, B=:=D -> Am1 is A-1, gen_path([location(Am1, B), location(A, B)|T], Result))
    ).

% checks if a location is valid. 
location(X, Y) :-
    between(0, 8, X),
    between(0, 8, Y).

% checks if a location is a covid zone.
covid_zone(location(X, Y)) :-
    covid(location(X, Y));
    (covid(location(A, B)), distance(location(X, Y), location(A, B), 1)).
    
% direct distance between two locations
distance(location(A, B), location(C, D), Result) :-
    Result is max(abs(A-C), abs(B-D)).
