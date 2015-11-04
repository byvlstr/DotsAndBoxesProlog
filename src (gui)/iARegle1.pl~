:-module(iARegle1,[ai/5]).

ai(Player,Board,Action,Index,Dim):-
    find(Board,Action,Index,Player,Dim).
    
find(Board,Action,Index,Player,Dim):-
    (   nth0(Index,Board,14);nth0(Index,Board,11);nth0(Index,Board,7);nth0(Index,Board,13)),!;
    findall(Ind,move(Action,Ind,Dim,[Player,Board,0],[_,_,_]),ListIndex),
    length(ListIndex,Length),
    random(0,Length,Pos),
    nth0(Pos,ListIndex,Index).
