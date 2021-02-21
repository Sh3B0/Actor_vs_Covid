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

% checks if a location is a covid zone.
% Opt: save results somewhere so you don't compute them multiple times.

% generates a valid map.
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

    home_m(H), covid1(C1), covid2(C2), protection1(P1), protection2(P2),
    \+ (H = C1; H = C2; H = P1; H = P2; C1 = C2; C1 = P2; C1 = P2; C2 = P1; C2 = P2; P1 = P2).