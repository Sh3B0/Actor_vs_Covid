% load common file for predicates, facts and rules used by both algoruthms
:- ['common.pl'].

% to reset facts, on each knowledge base load.
:- abolish(s/3).
:- abolish(answer/1).

% to indicate facts that will change dynamically.
:- dynamic(s/3).
:- dynamic(answer/1).

% constructs the knowledge base, i.e., builds the graph edges s(from, to, length) from the given map.
construct_kb :-
    location(A, B), location(C, D),
    distance(location(A, B), location(C, D), 1), \+ covid_zone(location(A, B)), \+ covid_zone(location(C, D)),
    assert(s(location(A, B), location(C, D), 1)).

% A* interface for user predicate, get the heruistic and calls the internal utility
astar(Start, End, _, Tmp):-
    heruistic(Start, End, H),
    astar_util([(H, H, 0, [Start])], End, _, Tmp).

% A* internal utility (base case, path constructed)
astar_util([(_, _, Tmp, [End|R])|_], End, [End|R], Tmp):-
    % write('Shortest path: '),
    % write([End|R]),
    assert(answer([End|R])).

% A* internal utility (recursive routine)
astar_util([(_, _, P, [X|R1])|R2], End, C, Tmp):-
    findall(
        (Sum, H1, NP, [Z,X|R1]),
        (s(X, Z, 1), not(member(Z, R1)), NP is P+1, heruistic(Z, End, H1), Sum is H1+NP),
        L
    ),
    append(R2, L, R3),
    sort(R3, R4),
    astar_util(R4, End, C, Tmp).

% uses diagonal distance routine, since the player can move in 8 directions.
heruistic(L1, L2, Her):- 
    distance(L1, L2, Her).

% applies algorithm, and writes results
start_astar :-
    home(H),
    findall(_, construct_kb, _),
    (
        (
            once(astar(location(8, 0), H, _, _)),
            answer(Path1),
            retract(answer(Path1)),
            length(Path1, Answer1)
        ); true
    ), % write(Answer1),nl,
    (
        (
            protection1(P1),
            once(astar(location(8, 0), P1, _, _)),
            answer(Path2_tmp),
            retract(answer(Path2_tmp)),
            gen_path(Path2_tmp, Path2),
            length(Path2, Answer2)
        ); true
    ), % write(Answer2),nl,
    (
        (
            protection2(P2),
            once(astar(location(8, 0), P2, _, _)),
            answer(Path3_tmp),
            retract(answer(Path3_tmp)),
            gen_path(Path3_tmp, Path3),
            length(Path3, Answer3)
        ); true
    ), %write(Answer3),nl,
    Spl is min(min(Answer1, Answer2), Answer3) - 1,
    reverse(Path1, Path1_r),
    reverse(Path2, Path2_r),
    reverse(Path3, Path3_r),
    write("Shortest path: "),
    (
        (Answer1-1 =:= Spl -> write(Path1_r); false);
        (Answer2-1 =:= Spl -> write(Path2_r); false);
        (Answer3-1 =:= Spl -> write(Path3_r); false)
    ),
    nl, write("Shortest path length: "), write(Spl), nl.

% starting point (map has to be defined before usage, check main.pl)
test_as :-
    catch(time(once(start_astar)), error(instantiation_error, _), (write("No path was found"), nl)).
