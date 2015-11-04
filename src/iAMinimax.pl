:-module(iAMinimax,[minimax/5]).

minimax(State, BestNextState , Val, 1, Dim) :-
	heuristique(State, Val, Dim).

minimax(State, BestNextState, Val, TreeCounter, Dim) :-
	addTreeCounter(TreeCounter, NextTreeCounter),
    getBoard(State, Board),
	bagof([Action, Index, NextState], move(Action, Index, Dim, Board, NextState), NextStateList),
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
	ValOne>ValTwo.
	
betterOf(State, _, _, StateTwo, ValTwo, StateTwo, ValTwo).
	
heuristique([_,_,[_,B,Score]], Val, Dim) :- not(Score = 0), Val is 1+(Score/1*(Dim*Dim)).
heuristique([_,_,[_,B,0]], Val, Dim) :- random(Val).

addTreeCounter(TreeCounter, NextTreeCounter) :- NextTreeCounter is TreeCounter + 1.

getBoard([_,_,Board],Board).
