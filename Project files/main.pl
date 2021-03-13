:- ['astar.pl'].
:- ['backtracking.pl'].

% Hard-coded maps for custom testing (check report for visualization)
% TO USE: uncomment the map and comment the second line 'once(get_random_map),' in rule 'test'

%% Map1
% home(location(1,1)).
% covid(location(4,1)).
% covid(location(1,6)).
% protection1(location(4,4)).
% protection2(location(7,7)).
% protection(location(4,4)).
% protection(location(7,7)).

%% Map2
% home(location(8,8)).
% covid(location(7,3)).
% covid(location(7,6)).
% protection1(location(0,6)).
% protection2(location(0,7)).
% protection(location(0,6)).
% protection(location(0,7)).

%% Map3
% home(location(1, 7)).
% covid(location(1, 5)).
% covid(location(3, 7)).
% protection1(location(1, 0)).
% protection2(location(8, 8)).
% protection(location(1, 0)).
% protection(location(8, 8)).

%% Map4
% home(location(8, 5)).
% covid(location(6, 5)).
% covid(location(8, 3)).
% protection1(location(3, 5)).
% protection2(location(8, 8)).
% protection(location(3, 5)).
% protection(location(8, 8)).

%% Map5
% home(location(1, 7)).
% covid(location(1, 5)).
% covid(location(3, 7)).
% protection1(location(0, 7)).
% protection2(location(0, 8)).
% protection(location(0, 7)).
% protection(location(0, 8)).

%% Map6
% home(location(1, 7)).
% covid(location(5, 1)).
% covid(location(7, 4)).
% protection1(location(0, 7)).
% protection2(location(0, 8)).
% protection(location(0, 7)).
% protection(location(0, 8)).

test :-
    ['main.pl'],
    once(get_random_map),
    nl, write("A* Search"), nl, write("======================"), nl,
    test_as,
    nl, write("Backtracking Search:"), nl, write("======================"), nl,
    test_bt.
