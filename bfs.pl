connected(a,b,1).
connected(a,c,1).
connected(a,d,1).
connected(c,e,1).
connected(c,f,1).
connected(d,f,1).

connected2(X,Y,D) :- connected(X,Y,D).
connected2(X,Y,D) :- connected(Y,X,D).

consed( A, B, [B|A]).

bfs(Goal, [[Goal|Visited]|_], Path):- 
    reverse([Goal|Visited], Path).

bfs( Goal, [Visited|Rest], Path) :-                     % take one from front
    Visited = [Start|_],            
    Start \== Goal,
    findall( X,
        ( connected2(X, Start, _), \+ member(X, Visited) ),
        [T|Extend]),
    maplist( consed(Visited), [T|Extend], VisitedExtended),      % make many
    append( Rest, VisitedExtended, UpdatedQueue),       % put them at the end
    bfs( Goal, UpdatedQueue, Path ).



breadth_first(Start, Goal, Path):- bfs( Goal, [[Start]], Path).