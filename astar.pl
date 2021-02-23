:- ['common.pl'].

:- abolish(s/3).
:- dynamic(s/3).

construct_kb :-
    location(A, B), location(C, D),
    distance(location(A, B), location(C, D), 1), \+ covid_zone(location(A, B)), \+ covid_zone(location(C, D)),
    assert(s(location(A, B), location(C, D), 1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% change naming
astar(Start,Final,_,Tp):-
      estimation(Start,Final,E),
      astar1([(E,E,0,[Start])],Final,_,Tp).

astar1([(_,_,Tp,[Final|R])|_],Final,[Final|R],Tp):- reverse([Final|R],L3),write('Shortest path: '),write(L3).

astar1([(_,_,P,[X|R1])|R2],Final,C,Tp):-
       findall((NewSum,E1,NP,[Z,X|R1]),(s(X,Z,V),
               not(member(Z,R1)),
               NP is P+V,
               estimation(Z,Final,E1),
               NewSum is E1+NP),L),
append(R2,L,R3),
sort(R3,R4),
astar1(R4,Final,C,Tp).


estimation(L1,L2,Est):- 
    distance(L1, L2, Est).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Hard-coded (impossible) map for custom testing
% TO USE: uncomment the following lines and comment the first line 'get_random_map' in predicate 'start_astar'

% home(location(1, 7)).
% covid(location(1, 5)).
% covid(location(3, 7)).
% protection(location(0, 7)).
% protection(location(0, 8)).


% edge case 
home(location(8, 5)).
covid(location(8, 3)).
covid(location(0, 8)).
protection(location(8, 1)).
protection(location(0, 0)).



start_astar :-
    %get_random_map,
    findall(_, construct_kb, _),
    (
        home(H),
        once(astar(location(8, 0), H, _, P1))
    ),
    % ;
    % (
    %     protection1(P),
    %     once(astar(location(8, 0), H, Tp, P2))
    % );
    % (
    %     protection2(P),
    %     once(astar(location(8, 0), H, Tp, P3))
    % ),
    % Spl is min(min(P1, P2), P3),
    nl, write("Shortest path length: "), write(P1), nl.

test_as :-
    ['astar.pl'],
    time(once(start_astar)).