:-module(iAMinimax,[minimax/5]).

minimax(State, BestNextState , Val, 3, Dim) :-
	heuristique(State, Val, Dim).

minimax(State, BestNextState, Val, TreeCounter, Dim) :-
	getPossibilities(State,Dim,TreeCounter,NextStateList,NextTreeCounter),
	best(State, NextStateList, BestNextState, Val, NextTreeCounter, Dim),!.

minimax(State, _ , Val, _, Dim) :-
	heuristique(State, Val, Dim).
	
best(State, [StateOne], State, Val, _, Dim) :-
	minimax(StateOne, _ , Val, _, Dim).
	
best(State, [StateOne|NextStateList], BestState, BestVal, TreeCounter, Dim) :-
	minimax(StateOne, _, ValOne, TreeCounter, Dim),
	best(State, NextStateList, StateTwo, ValTwo, TreeCounter, Dim),
	betterOf(State, StateOne, ValOne, StateTwo, ValTwo, BestState, BestVal).
	
betterOf(State, StateOne, ValOne, _, ValTwo, StateOne, ValOne) :-
	playerComputer(State),
	ValOne>ValTwo.
	
betterOf(State, _, _, StateTwo, ValTwo, StateTwo, ValTwo).
	
heuristique([_,_,[_,B,Score]], Val, Dim) :- not(Score = 0), Val is 1+(Score/(Dim*Dim)).
heuristique([_,_,[_,B,0]], Val, Dim) :- random(Val).

addTreeCounter(TreeCounter, Number, NextTreeCounter) :- NextTreeCounter is TreeCounter + Number.

getBoard([_,_,Board],Board).

dispatchBoard([Player,B,Score],Player,B,Score).

playerComputer([_,_,[computer,_,_]]).

getPossibilities(State,Dim,TreeCounter,NextStateList,NextTreeCounter):-
    getBoard(State, Board),
    dispatchBoard(Board,Player,B,Score),
    addTreeCounter(TreeCounter, 1, NextTreeCounter),
    bagof([Action, Index, [Player, B2, Score2]], move(Action, Index, Dim, [Player, B, Score] , [Player, B2, Score2]), NextStateList)
	;
    getBoard(State, Board),
    addTreeCounter(TreeCounter, 3, NextTreeCounter),
    findall([Action, Index, NextState], move(Action, Index, Dim, Board, NextState), NextStateList).
