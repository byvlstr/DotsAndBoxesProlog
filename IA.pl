:- module(minimax, [minimax/3]).

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax(Pos, BestNextPos, Val, TreeCounter) :-                     % Pos has successors
    TreeCounter < 4,
    addTreeCounter(TreeCounter, NextTreeCounter),
    bagof(NextPos, move(Action,Index,Pos, NextPos), NextPosList), % We get all possible NextPos from move
    best(NextPosList, BestNextPos, Val, NextTreeCounter), !.



minimax(Pos, _, Val, _) :-                     % Pos has no successors
    utility(Pos, Val).						   % End of the tree

best([Pos], Pos, Val, TreeCounter) :-
    minimax(Pos, _, Val, TreeCounter), !.

best([Pos1 | PosList], BestPos, BestVal, TreeCounter) :-
    minimax(Pos1, _, Val1, TreeCounter),
    best(PosList, Pos2, Val2, TreeCounter),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 < Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 < Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

% min_to_move(+Pos)
% True if the next player to play is the MIN player.
min_to_move([user, _, _]).

% max_to_move(+Pos)
% True if the next player to play is the MAX player.
max_to_move([computer, _, _]).


%endPos(Player,Board)
endPos([X1,X2,X3,X4,X5,X6,X7,X8,X9]):-
	equal(X1,X2,X3,X4,X5,X6,X7,X8,X9),
    X1 = 15,
    X2 = 15,
    X3 = 15,
    X4 = 15,
    X5 = 15,
    X6 = 15,
    X7 = 15,
    X8 = 15,
    X9 = 15.

% utility(Pos, Val)
% This will return the proper Val for each Pos
utility([user,win,_],1). %toto win
utility([computer,win,_],1). %titi win
utility([_,draw,_],0). %no one win
utility([user,B,Score], Val) :- Val is Score/9.
utility([computer,B,Score], Val) :- Val is Score/9.

% flatList(List[List], List)
% This will override flatten, but it works only for
% a list composed with a single element.
flatList([L],L).
flatList(L,L).

% boardH(Board, BoardH)
% This function will return the heuristic value 
% associated with each position of the board.
boardH([],[]).
boardH([L1|L2],BH) :- flatList(L1,L1f), sumBits(L1f, S), boardH(L2,BH1), addList(S,BH1,BH).
% sumBits(L,sum)
% This function computes the sum of all bits in L
sumBits([],0).
sumBits([X|L], S) :- sumBits(L, S1), S is S1 + X.
      
%equal True if they are the same
%Opti à faire : equal pour dimension illimitée
equal(X,X,X,X,X,X,X,X,X).

% addTreeCounter(TreeCounter,NewTreeCounter)
% This function will be used to stop the search in the tree
addTreeCounter(X,Y) :- Y is X + 1.

% addList(X, List, List+X)
% This function will add X to the List
addList(X,L,[X|L]).

