% :- consult('map.pl').
% :- consult('backtracking.pl').
% :- abolish(dist/1).
% :- dynamic(dist/2).


% neighbouring(L1, L2) :-
%     distance(L1, L2, 1), \+ covid_zone(L1), \+ covid_zone(L2).

% % sample graph.

% arc(1,0).
% arc(8,10). 
% arc(2,7). 
% arc(1,10).
% arc(10,8).
% arc(9,0). 
% arc(1,4). 
% arc(4,6). 
% arc(5,4). 
% arc(X, Y) :- arc(Y, X).

% dist(0, X) :- X = 0.

% bfs(_, []) :- true.

% % bfs from source, with seen list, and the queue
% bfs(Seen, [S|QueueRest]) :-
%     write(S), nl, % we are currently visiting S

%     arc(S, U), \+ member(U, Seen),  % forall nodes U that are directly connected to S
%     (
%         % if U was not seen before;
%         \+ member(U, Seen) -> 
%         (
%             % mark it as seen
%             append([U], Seen, Seen2),

%             % update distance to U
%             dist(S, DtoS),
%             DtoSp1 is DtoS + 1,
%             assert(dist(U, DtoSp1)),

%             % put it in the queue
%             append(QueueRest, [U], Queue2)
%         ) ; true
%     ), bfs(Seen2, Queue2).

% start :-
%     bfs([0], [0]).

% % source is node 0, it's the only seen node, queue contains it only initially 

% /* Probably not the best thing to do, but here is the reference.

% void bfs(int x){

%     for(int i=0;i<n;i++){
%         dist[i]=-1;
%     }

%     queue<int>q;
%     q.push(x);
%     seen[x]=1;
%     dist[x]=0;

%     while(!q.empty()){
%         int cur=q.front();

%         cout<<cur<<" ";

%         q.pop();
%         for(auto u:g[cur]){
%             if(!seen[u]){
%                 seen[u]=1;
%                 dist[u] = dist[cur] + 1;
%                 q.push(u);
%             }
%         }
%     }
% }
% */



