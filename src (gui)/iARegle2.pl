:-module(iARegle2,[ai2/5]).

%This not so dumb AI follows two rules
%Complete the last edge of a box if possible
%Otherwise, do not create a box with 3 edges
%Otherwise, take a random action
ai2(Player,Board,Action,Index,Dim):-
    find(Board,Action,Index,Player,Dim).
    
find(Board,Action,Index,Player,Dim):-
    (   nth0(Index,Board,14);nth0(Index,Board,11);nth0(Index,Board,7);nth0(Index,Board,13)),!;
    findall([Act,Ind],move(Act,Ind,Dim,[Player,Board,0],[_,_,_]),MovePool),
	random_permutation(MovePool,Pool),
    chooseMove(Pool,Board,Dim,Pool,Action,Index).

%chooses a move that follows the second rule
%iterates the whole list
chooseMove([Head|Tail],Board,Dim,Pool,Action,Index):-
    nth0(0,Head,Act),
    nth0(1,Head,Ind),
	moveaux(Board,Act,Ind,NewBoard,Dim,NewValue,NeighValue),
    ( checkThreeSides(NewValue),checkThreeSides(NeighValue),Action = Act,Index = Ind;
      chooseMove(Tail,Board,Dim,Pool,Action,Index)
    ).

%List contains no moves that satisfy the second rule
chooseMove([],Board,Dim,Pool,Action,Index):-
    length(Pool,Length),
    random(0,Length,Pos),
    nth0(Pos,Pool,Move),
    nth0(0,Move,Action),
    nth0(1,Move,Index).
	

%To check if a box is surrounded by three sides
checkThreeSides(Value):-
    Value =\= 14,
    Value =\= 13,
    Value =\= 11,
    Value =\= 7.
