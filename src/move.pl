:-module(move,[move/5,score/5,moveaux/7]).

%(edge filled,value)
%for filling in edges, corresponds to value that needs to be added 
action(bottom,8).
action(top,2).
action(left,4).
action(right,1).
action(null,0).

%each action taken has a corresponding action
%to be taken by the neighbour
reaction(bottom,top).
reaction(top,bottom).
reaction(left,right).
reaction(right,left).


%move predicate
move(Action,Index,Dim,[Player,Board,OldScore],[NextPlayer,NewBoard,NewScore]):-
    checkMove(Action,Board,Index),
    moveaux(Board,Action,Index,NewBoard,Dim,NewValue,NeighValue),
    score([NewValue,NeighValue],OldScore,NewScore,Player,NextPlayer).

%predicate that executes the move on our list
moveaux(Board,Action,Index,NewBoard,Dim,NewValue,NeighValue):-
    action(Action,Bit),
    nth0(Index,Board,CurrentValue),
    NewValue is CurrentValue + Bit,
    
    neighbour(Action,Index,Dim,Reaction,NeighIndex),
    action(Reaction,Bit2),
    (   nth0(NeighIndex,Board,NValue);NValue = 0),
    NeighValue is NValue + Bit2,
    checkMove(Action,Board,Index),
    gameReplace(NewValue,NeighValue
                ,Index,NeighIndex,Board,NewBoard),!.

%predicate to obtain what neighbour to update
%and what value to update it with
neighbour(Action,Index,Dim,Reaction,NeighIndex):-
    
    ( Action = bottom,NeighIndex is Index + Dim,
        not(border(Action,Index,Dim)),reaction(Action,Reaction)  );
    ( Action = top,NeighIndex is Index - Dim,
        not(border(Action,Index,Dim)),reaction(Action,Reaction)  );
    ( Action = left,NeighIndex is Index - 1,
        not(border(Action,Index,Dim)),reaction(Action,Reaction)  );
    ( Action = right,NeighIndex is Index + 1,
        not(border(Action,Index,Dim)),reaction(Action,Reaction)  );    
    
    ( NeighIndex = -1,Reaction = null  ).

%predicate that returns true if the move is a border case
border(Action,Index,Dim):-
    (  Action = bottom, checkBottom(Index,Dim) );
    (  Action = top,checkTop(Index,Dim) );
    (  Action = left,checkLeft(Index,Dim) );
    (  Action = right,checkRight(Index,Dim) ).

checkBottom(Index,Dim):-
    Dimension is Dim*Dim,
    Index >= Dimension - Dim.

checkTop(Index,Dim):-
    Index =< Dim - 1.

checkLeft(Index,Dim):-
    Check is Index mod Dim,
    Check is 0.

checkRight(Index,Dim):-
    Check is Index mod Dim,
    Check is Dim - 1.


%predicate to check if an edge has already been filled previously
%returns true if move is legal
checkMove(Action,Board,Index):-
    nth0(Index,Board,CurrentValue),
    action(Action,Bit),
    check(CurrentValue,Bit).

check(CurrentValue,Bit):-
    Check is CurrentValue /\ Bit,
    not(Check is Bit).

% Calculate Score
score([Value1,Value2],OldScore,NewScore,CurrentPlayer,NextPlayer) :- 
    	(   Value1 = 15, Value2 = 15 ),
    	NewScore is OldScore + 2,!,
    	NextPlayer = CurrentPlayer;
    	(   Value1 = 15; Value2 = 15 ), 
    	NewScore is OldScore + 1,!, 
    	NextPlayer = CurrentPlayer;
    	NewScore is OldScore, nextPlayer(CurrentPlayer,NextPlayer).



% Alternate Players
nextPlayer(user,computer).
nextPlayer(computer,user).
