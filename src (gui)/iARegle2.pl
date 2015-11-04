:-module(iARegle2,[ai/5]).

ai(Player,Board,Action,Index,Dim):-
    find(Board,Action,Index,Player,Dim).
    
find(Board,Action,Index,Player,Dim):-
    (   nth0(Index,Board,14);nth0(Index,Board,11);nth0(Index,Board,7);nth0(Index,Board,13)),!;
    findall([Act,Ind],move(Act,Ind,Dim,[Player,Board,0],[_,_,_]),MovePool),
	random_permutation(MovePool,Pool),
    chooseMove(Pool,Board,Dim,Pool,Action,Index).

chooseMove([Head|Tail],Board,Dim,Pool,Action,Index):-
    nth0(0,Head,Act),
    nth0(1,Head,Ind),
	moveaux(Board,Act,Ind,NewBoard,Dim,NewValue,NeighValue),
    ( checkThreeSides(NewValue),checkThreeSides(NeighValue),Action = Act,Index = Ind;
      chooseMove(Tail,Board,Dim,Pool,Action,Index)
    ).

chooseMove([],Board,Dim,Pool,Action,Index):-
    length(Pool,Length),
    random(0,Length,Pos),
    nth0(Pos,Pool,Move),
    nth0(0,Move,Action),
    nth0(1,Move,Index).
	

checkThreeSides(Value):-
    Value =\= 14,
    Value =\= 13,
    Value =\= 11,
    Value =\= 7.
