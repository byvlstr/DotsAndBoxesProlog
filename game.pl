%predicate to display our board
displayBoard(L,UserScore,ComputerScore,Dim):-
    write('************ GAME **************'),nl,
    displayMatrix(L,Dim),nl,
    
    write('Your Score : '), write(UserScore), write(' | Computer Score : '),
    write(ComputerScore), nl.

%removes a row of our dimension size from the list
split(List,List1,List2,Dim):-
    append(List1,List2,List),
    length(List1,Dim).
    
%prints our list in matrix form
displayMatrix([],Dim).
displayMatrix(List,Dim):-
    write('|'),
    split(List,List1,Others,Dim),
    displayRow(List1),
    displayMatrix(Others,Dim).

%prints the row
displayRow([]):-nl.
displayRow([Head|Tail]):-
    write(' '),write(Head),write(' |'),
    displayRow(Tail).

% Predicate Display the winner 
displayWinner(UserScore,ComputerScore) :-
    % User is the winner
    UserScore > ComputerScore,
    write('You win :)');
    
    % Computer is the winner
    UserScore < ComputerScore,
    write('You lost :(');
    
    % Equality
    write('Draw :-').

% Include files
% Creation de la liste
createList([],_,0).
createList([X],X,1).
createList([X|L],X,N) :- plus(S,1,N), createList(L, X, S).

% Remplacer un element dans la liste
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% A replace that deals with two cases, easier for our game
% do not replace a value of -1
gameReplace(Value1,Value2,Index1,Index2,List,NewList):-
    (   replace(List,Index1,Value1,TmpList);TmpList = List),
    (   replace(TmpList,Index2,Value2,NewList);true),!.



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

% Verify if its finished
gameOver(UserScore, ComputerScore,Dim) :- 
    Total is UserScore + ComputerScore,
    MaxScore is Dim*Dim,
    Total = MaxScore.

% Alternate Players
nextPlayer(user,computer).
nextPlayer(computer,user).

% Prédicate of play/4
% +Board@ list: board game
% +UserScore@ integer: user score (Incremented by one)
% +ComputerScore@ integer: computer score (Incremented by one)
% +Player@ const: player turn [ user | computer ] 
play(Board,UserScore,ComputerScore,Player,Dim) :-
    % Verify if its the game is finished 
    gameOver(UserScore,ComputerScore,Dim),
    displayWinner(UserScore,ComputerScore);
    
    % Display the board
    displayBoard(Board,UserScore,ComputerScore,Dim),
    
    % User turn
	Player = user, 		
		computerPlay(Board,NewBoard,UserScore,NewUserScore,NextPlayer,Dim),
    	play(NewBoard,NewUserScore,ComputerScore,NextPlayer,Dim);
    
    % Computer turn
    Player = computer, 	
    	computerPlay(Board,NewBoard,ComputerScore,NewComputerScore,NextPlayer,Dim),
    	play(NewBoard,UserScore,NewComputerScore,NextPlayer,Dim).


% Predicate user player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] its the next player turn
userPlay(Board,NewBoard,OldScore,NewScore,NextPlayer,Dim) :- 
	write('Play : '), 
	read(Action), read(Index),
    move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]).

% Predicate computer player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] its the next player turn
computerPlay(Board,NewBoard,OldScore,NewScore,NextPlayer,Dim) :- 
    write('Computer playing...'), nl,
    %minimax([computer,Board,OldScore],[_,_,_],_,0,Action,Index),
    ai(computer,Board,Action,Index,Dim),
    move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]).


ai(Player,Board,Action,Index,Dim):-
    find(Board,Action,Index,Player,Dim).
    
find(Board,Action,Index,Player,Dim):-
    (   nth0(Index,Board,14);nth0(Index,Board,11);nth0(Index,Board,7);nth0(Index,Board,13)),!;
    findall(Ind,move(Action,Ind,Dim,[Player,Board,0],[_,_,_]),ListIndex),
    length(ListIndex,Length),
    random(0,Length,Pos),
    nth0(Pos,ListIndex,Index).	


game(Dim) :-
    	Dimension is Dim*Dim,
        createList(Board,0,Dimension),
		play(Board,0,0,user,Dim).
    



    
    
    
    
    
    
    

