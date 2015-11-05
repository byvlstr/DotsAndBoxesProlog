:-module(iAMinimax,[minimax/6]).

minimax(State, BestNextState , Val, 2, Dim, PlayerBefore) :-
	heuristique(State, Val, Dim, PlayerBefore).

minimax(State, BestNextState, Val, TreeCounter, Dim,_) :-
	getPossibilities(State,Dim,TreeCounter,NextStateList,NextTreeCounter),
	best(State, NextStateList, BestNextState, Val, NextTreeCounter, Dim),!.

minimax(State, _ , Val, _, Dim, PlayerBefore) :-
	heuristique(State, Val, Dim, PlayerBefore).
	
best(State, [StateOne], StateOne, Val, _, Dim) :-
    getPlayer(State,PlayerBefore),
	minimax(StateOne, _ , Val, _, Dim,PlayerBefore).
	
best(State, [StateOne|NextStateList], BestState, BestVal, TreeCounter, Dim) :-
	getPlayer(State,PlayerBefore),
    minimax(StateOne, _, ValOne, TreeCounter, Dim,PlayerBefore),
	best(State, NextStateList, StateTwo, ValTwo, TreeCounter, Dim),
	betterOf(State, StateOne, ValOne, StateTwo, ValTwo, BestState, BestVal).
	
betterOf(State, StateOne, ValOne, _, ValTwo, StateOne, ValOne) :-
	ValOne>ValTwo.
	
betterOf(_, _, _, StateTwo, ValTwo, StateTwo, ValTwo).
	
heuristique([_,_,[_,B,Score]], Val, Dim, computer) :- not(Score = 0), Val is 1+(Score/(Dim*Dim)).
heuristique([_,_,[_,B,Score]], Val, Dim, user) :- not(Score = 0), Val is -1-(Score/(Dim*Dim)).
heuristique([_,_,[_,B,0]], Val, Dim,_) :- random(Val).

addTreeCounter(TreeCounter, NextTreeCounter) :- NextTreeCounter is TreeCounter + 1.

getBoard([_,_,Board],Board).

getPlayer(State,Player) :- getBoard(State,Board),dispatchBoard(Board,Player,_,_).

dispatchBoard([Player,B,Score],Player,B,Score).

playerComputer([_,_,[computer,_,_]]).
playerPlayer([_,_,[user,_,_]]).

getPossibilities(State,Dim,TreeCounter,NextStateList,NextTreeCounter):-
    getBoard(State, Board),
    dispatchBoard(Board,Player,B,Score),
    bagof([Action, Index, [Player, B2, Score2]], move(Action, Index, Dim, [Player, B, Score] , [Player, B2, Score2]), NextStateList)
	;
    getBoard(State, Board),
    addTreeCounter(TreeCounter, NextTreeCounter),
    findall([Action, Index, NextState], move(Action, Index, Dim, Board, NextState), NextStateList).
