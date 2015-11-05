%This IA uses the minimax algorithm to play.
%The idea is to determinate the available moves, and then pick the one which provides
%the best score for the computer.
:-module(iAMinimax,[minimax/6]).

%Minimax with the TreeCounter at 2 : when the algorithm is playing two plays ahead,
%we stop the search, and determinate the value of the board.
%+ State : the current State of the game
%+ Dim : Dimension of the board
%+ PlayerBefore : The player who plays the earlier turn
%- BestNextState : not used
%- Val : value of the play.
minimax(State, BestNextState , Val, 2, Dim, PlayerBefore) :-
        heuristique(State, Val, Dim, PlayerBefore).

%Minimax at start : it gets every plays available, then search for the best.
%This predicate is call recursively in the "best" predicate.
%+ State : the current State of the game
%+ Dim : Dimension of the board
%+ TreeCounter : This counter is the depth of our search in the tree.
%- BestNextState : The play the computer will do.
%- Val : value of the play.
minimax(State, BestNextState, Val, TreeCounter, Dim,_) :-
        getPossibilities(State,Dim,TreeCounter,NextStateList,NextTreeCounter),
        best(State, NextStateList, BestNextState, Val, NextTreeCounter, Dim),!.

%Minimax at the end of the tree.
%+ State : the current State of the game
%+ Dim : Dimension of the board
%+ PlayerBefore : The player who plays the earlier turn
%- BestNextState : not used
%- Val : value of the play.
minimax(State, _ , Val, _, Dim, PlayerBefore) :-
        heuristique(State, Val, Dim, PlayerBefore).
        
%Best predicate when the list is at the end.
best(State, [StateOne], StateOne, Val, _, Dim) :-
    getPlayer(State,PlayerBefore),
        minimax(StateOne, _ , Val, _, Dim,PlayerBefore).

%Best predicate will determinate which plays avalaible is the best to do.
%+ State : the current State of the game
%+ NextStateList : the list of the possible plays.
%- BestState: the best play to do
%- BestVal : Val associated to the best play.
%+ TreeCounter : depth in the tree
%+ Dim : Dimension of the board  
best(State, [StateOne|NextStateList], BestState, BestVal, TreeCounter, Dim) :-
        getPlayer(State,PlayerBefore),
    minimax(StateOne, _, ValOne, TreeCounter, Dim,PlayerBefore),
        best(State, NextStateList, StateTwo, ValTwo, TreeCounter, Dim),
        betterOf(State, StateOne, ValOne, StateTwo, ValTwo, BestState, BestVal).

%betterOf chooses the best play between two. If the value of the first is superior, first is chosen,
%othewise it is the second.   
betterOf(State, StateOne, ValOne, _, ValTwo, StateOne, ValOne) :-
        ValOne>ValTwo.
        
betterOf(_, _, _, StateTwo, ValTwo, StateTwo, ValTwo).
       
%heuristique applies a value to a play. This value is real (not estimated with the end position),
%and it determines if the play is good or not for the computer to play. 
heuristique([_,_,[_,B,Score]], Val, Dim, computer) :- not(Score = 0), Val is 1+(Score/(Dim*Dim)).
heuristique([_,_,[_,B,Score]], Val, Dim, user) :- not(Score = 0), Val is -1-(Score/(Dim*Dim)).
heuristique([_,_,[_,B,0]], Val, Dim,_) :- random(Val).

%addTreeCounter simply add one to the depth counter.
addTreeCounter(TreeCounter, NextTreeCounter) :- NextTreeCounter is TreeCounter + 1.

%getBoard will give the board ([Player, B, Score]) part of a state.
getBoard([_,_,Board],Board).

%getPlayer will give the player wich has to play in the state given.
getPlayer(State,Player) :- getBoard(State,Board),dispatchBoard(Board,Player,_,_).

%dispatchBoard give the board as its three components : PLayer, B, Score.
dispatchBoard([Player,B,Score],Player,B,Score).

%These predicates are true for the player which is associated to.
playerComputer([_,_,[computer,_,_]]).
playerPlayer([_,_,[user,_,_]]).

%getPossibilities will find all the possible plays for the computer, using the bagof
%function. First of all, it searches the moves considered as obvious : moves that give one point to the player. If there is
% no possibilities, then it will find all the other.
getPossibilities(State,Dim,TreeCounter,NextStateList,NextTreeCounter):-
    getBoard(State, Board),
    dispatchBoard(Board,Player,B,Score),
    bagof([Action, Index, [Player, B2, Score2]], move(Action, Index, Dim, [Player, B, Score] , [Player, B2, Score2]), NextStateList)
        ;
    getBoard(State, Board),
    addTreeCounter(TreeCounter, NextTreeCounter),
    findall([Action, Index, NextState], move(Action, Index, Dim, Board, NextState), NextStateList).