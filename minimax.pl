% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax(Pos, BestNextPos, Val, 3, Player) :-                     % Pos has successors
	utility(Pos, Val, Player).

minimax(Pos, BestNextPos, Val, TreeCounter, _) :-                     % Pos has successors
    getPlayer(Pos, PlayerPos),
    addTreeCounter(TreeCounter, NextTreeCounter),
    findall(NextPos, move(Action,Index,Pos, NextPos), NextPosList), % We get all possible NextPos from move
    best(NextPosList, BestNextPos, Val, NextTreeCounter, PlayerPos),!.

minimax(Pos, _, Val, _, Player) :-                     % Pos has no successors
    utility(Pos, Val, Player).						   % End of the tree

best([Pos], Pos, Val, TreeCounter, Player) :-
    minimax(Pos, _, Val, TreeCounter, Player), !.

best([Pos1 | PosList], BestPos, BestVal, TreeCounter, Player) :-
    minimax(Pos1, _, Val1, TreeCounter, Player),
    best(PosList, Pos2, Val2, TreeCounter, Player),
    betterOf(Pos1, Val1, Pos2, Val2, Player, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Player, Pos0, Val0) :-   % Pos0 better than Pos1	
    min_to_move(Pos0), max_now(Player)                        % MIN to move in Pos0
    Val0 > Val1, !                             % MAX prefers the greater value
    ;
    min_to_move(Pos0), min_now(Player)
    Val0 < Val1, !
	;   
    max_to_move(Pos0), max_now(Player)                        % MAX to move in Pos0
    Val0 > Val1, !                           % MIN prefers the lesser value
	;   
	max_to_move(Pos0), min_now(Player)
	Val0 < Val1, !.

betterOf(_, _, Pos1, Val1, Player, Pos1, Val1).        % Otherwise Pos1 better than Pos0

% min_to_move(+Pos)
% True if the next player to play is the MIN player.
min_to_move([user, _, _]).

min_now(user).

% max_to_move(+Pos)
% True if the next player to play is the MAX player.
max_to_move([computer, _, _]).

max_now(computer).

getPlayer(Pos, Player) :- nth0(0, Pos, Player).

%simple heuristic that takes the potential score and divides it by the total score
utility([user,B,Score], Val, Player) :- Val is Score/9.
utility([computer,B,Score], Val, Player) :- Val is Score/9.