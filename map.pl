:- abolish(home_m/1).
:- abolish(covid1/1).
:- abolish(covid2/1).
:- abolish(protection1/1).
:- abolish(protection2/1).

:- dynamic(home_m/1).
:- dynamic(covid1/1).
:- dynamic(covid2/1).
:- dynamic(protection1/1).
:- dynamic(protection2/1).

actor(location(8, 0)).

% generates a map with no collisions (but may be invalid).
gen_map :-
    random_between(0, 8, Hx),
    random_between(0, 8, Hy), 
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
    (
        (
            H = C1; H = C2; H = P1; H = P2; C1 = C2; C1 = P1; C1 = P2; C2 = P1; C2 = P2; P1 = P2; H = A; P1 = A; P2 = A; C1 = A; C2 = A; 
            distance(H, C1, 1); distance(H, C2, 1); distance(P1, C1, 1); distance(P2, C1, 1); distance(P1, C2, 1); distance(P2, C2, 1);
            distance(location(8, 0), C1, 1); distance(location(8, 0), C2, 1) 
        ) -> (retract(home_m(H)), retract(covid1(C1)), retract(covid2(C2)), retract(protection1(P1)), retract(protection2(P2)), gen_map) ; true
    ).

distance(location(A, B), location(C, D), Result) :-
    Result is max(abs(A-C), abs(B-D)).
