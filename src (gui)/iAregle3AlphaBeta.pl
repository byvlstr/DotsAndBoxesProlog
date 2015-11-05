

% Predicate verify if player wins
winner(UserScore, ComputerScore, Dim, computer) :- 
   gameOver(UserScore,ComputerScore,Dim), UserScore < ComputerScore.

% Predicate verify if player winn 
winner(UserScore, ComputerScore, Dim, user) :- 
   gameOver(UserScore,ComputerScore,Dim), UserScore > ComputerScore.

% Predicate verify if player winn 
winner(UserScore, ComputerScore, Dim, null) :- 
   gameOver(UserScore,ComputerScore,Dim), UserScore = ComputerScore.

value([UserScore, ComputerScore],100,Dim) :-
    winner(UserScore, ComputerScore, Dim, computer), !.
value([UserScore, ComputerScore],-100,Dim) :-
    winner(UserScore, ComputerScore, Dim, user), !.
value([UserScore, ComputerScore],0,Dim) :-
    winner(UserScore, ComputerScore, Dim, null), !.

value([UserScore, ComputerScore],V,_) :- 
   V is ComputerScore - UserScore.

scorePlayer(user,[UScore,_],UScore).
scorePlayer(computer,[_,CScore],CScore).

updateScore(computer,[UScore,_,NScore],[UScore,NScore]).
updateScore(user,[_,CScore,NScore],[NScore,CScore]).

% Player,Depth,[UScore,CScore,Board],Alpha,Beta,[Ind,Act],Value,Dim
alpha_beta(_Player,0,[UScore,CScore,_],_Alpha,_Beta,[_,_],Value,Dim) :- 
   value([UScore,CScore],Value,Dim).

alpha_beta(Player,Depth,[UScore,CScore,Board],Alpha,Beta,[Ind,Act],Value,Dim) :- 
   Depth > 0, 
   findall([Ind1,Act1],move(Act1,Ind1,Dim,[Player,Board,0],[_,_,_]),Moves), 
   Alpha1 is -Beta, % max/min
   Beta1 is -Alpha,
   D1 is Depth-1, 
   evaluateAndChoose(Player,Moves,[UScore,CScore,Board],Dim,D1,Alpha1,Beta1,nil,([Ind,Act],Value)).

evaluateAndChoose(Player,[[Ind,Act]|Moves],[UScore,CScore,Board],Dim,D,Alpha,Beta,Record,BestMove) :-
   	scorePlayer(Player, [UScore,CScore], OScore),
    move(Act,Ind,Dim,[Player,Board,OScore],[NextP,NBoard,NScore]), 
    %nextPlayer(Player,NextP),
	updateScore(NextP, [UScore,CScore,NScore],[NUScore,NCScore]),
    
    alpha_beta(NextP,D,[NUScore,NCScore,NBoard],Alpha,Beta,_OtherMove,Value,Dim),
    Value1 is -Value,
    testBest(Player,[Ind,Act],Value1,Dim,D,Alpha,Beta,Moves,[UScore,CScore,Board],Record,BestMove).

evaluateAndChoose(_Player,[],[_,_,_],_,_D,Alpha,_Beta,[Ind,Act],([Ind,Act],Alpha)).


testBest(_Player,[Index,Act],Value,_,_D,_Alpha,Beta,_Moves,[_,_,_],_Record,([Index,Act],Value)) :- 
   Value >= Beta, !.

testBest(Player,[Ind,Act],Value,Dim,D,Alpha,Beta,Moves,[UScore,CScore,Board],_Record,BestMove) :- 
   Alpha < Value, Value < Beta, !, 
   evaluateAndChoose(Player,Moves,[UScore,CScore,Board],Dim,D,Value,Beta,[Ind,Act],BestMove).

testBest(Player,_Move,Value,Dim,D,Alpha,Beta,Moves,[UScore,CScore,Board],Record,BestMove) :- 
   Value =< Alpha, !, 
   evaluateAndChoose(Player,Moves,[UScore,CScore,Board],Dim,D,Alpha,Beta,Record,BestMove).

aiAlpha(Player,Board,Action,Index,Dim,[UserScore,ComputerScore]):-
    alpha_beta(Player,2,[UserScore,ComputerScore,Board],-200,200,[Action,Index],_,Dim).
