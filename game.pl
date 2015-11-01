:- module(minimax, [minimax/3]).

% minimax(Pos, BestNextPos, Val)
% Pos is a position, Val is its minimax value.
% Best move from Pos leads to position BestNextPos.
minimax(Pos, BestNextPos, Val, 3) :-                     % Pos has successors
	utility(Pos, Val).

minimax(Pos, BestNextPos, Val, TreeCounter) :-                     % Pos has successors
    addTreeCounter(TreeCounter, NextTreeCounter),
    findall(NextPos, move(Action,Index,Pos, NextPos), NextPosList), % We get all possible NextPos from move
    best(NextPosList, BestNextPos, Val, NextTreeCounter),!.

minimax(Pos, _, Val, _) :-                     % Pos has no successors
    utility(Pos, Val).						   % End of the tree

best([Pos], Pos, Val, TreeCounter) :-
    minimax(Pos, _, Val, TreeCounter), !.

best([Pos1 | PosList], BestPos, BestVal, TreeCounter) :-
    minimax(Pos1, _, Val1, TreeCounter),
    best(PosList, Pos2, Val2, TreeCounter),
    betterOf(Pos1, Val1, Pos2, Val2, BestPos, BestVal).

betterOf(Pos0, Val0, _, Val1, Pos0, Val0) :-   % Pos0 better than Pos1
    min_to_move(Pos0),                         % MIN to move in Pos0
    Val0 < Val1, !                             % MAX prefers the greater value
    ;
    max_to_move(Pos0),                         % MAX to move in Pos0
    Val0 > Val1, !.                            % MIN prefers the lesser value

betterOf(_, _, Pos1, Val1, Pos1, Val1).        % Otherwise Pos1 better than Pos0

% min_to_move(+Pos)
% True if the next player to play is the MIN player.
min_to_move([user, _, _]).

% max_to_move(+Pos)
% True if the next player to play is the MAX player.
max_to_move([computer, _, _]).


%endPos(Player,Board)
endPos([X1,X2,X3,X4,X5,X6,X7,X8,X9]):-
	equal(X1,X2,X3,X4,X5,X6,X7,X8,X9),
    X1 = 15,
    X2 = 15,
    X3 = 15,
    X4 = 15,
    X5 = 15,
    X6 = 15,
    X7 = 15,
    X8 = 15,
    X9 = 15.

% utility(Pos, Val)
% This will return the proper Val for each Pos
utility([user,win,_],1). %toto win
utility([computer,win,_],1). %titi win
utility([_,draw,_],0). %no one win

%simple heuristic that takes the potential score and divides it by the total score
utility([user,B,Score], Val) :- Val is Score/9.
utility([computer,B,Score], Val) :- Val is Score/9.
utility([_,B,0],0).

boardH(
      
%equal True if they are the same
%Opti à faire : equal pour dimension illimitée
equal(X,X,X,X,X,X,X,X,X).

% addTreeCounter(TreeCounter,NewTreeCounter)
% This function will be used to stop the search in the tree
addTreeCounter(X,Y) :- Y is X + 1.

% addList(X, List, List+X)
% This function will add X to the List
addList(X,L,[X|L]).

game :-
        createList(Board,0,9),
		play(Board,0,0,user).

% Prédicate of play/4
% +Board@ list: board game
% +UserScore@ integer: user score (Incremented by one)
% +ComputerScore@ integer: computer score (Incremented by one)
% +Player@ const: player turn [ user | computer ] 
play(Board,UserScore,ComputerScore,Player) :-
    % Verify if it's the game is finished 
    gameOver(UserScore,ComputerScore),
    displayWinner(UserScore,ComputerScore);
    
    % Display the board
    displayBoard(Board,UserScore,ComputerScore),
    
    % User turn
	Player = user, 		
		userPlay(Board,NewBoard,UserScore,NewUserScore,NextPlayer),
    	play(NewBoard,NewUserScore,ComputerScore,NextPlayer);
    
    % Computer turn
    Player = computer, 	
    	computerPlay(Board,NewBoard,ComputerScore,NewComputerScore,NextPlayer),
    	play(NewBoard,UserScore,NewComputerScore,NextPlayer).


% Predicate user player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] it's the next player turn
userPlay(Board,NewBoard,OldScore,NewScore,NextPlayer) :- 
	write('Play : '), 
	read(Action), read(Index),
    move(Action,Index,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]).

% Predicate computer player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] it's the next player turn
computerPlay(Board,NewBoard,OldScore,NewScore,NextPlayer) :- 
    write('Computer playing...'), nl, 
    minimax([computer,Board,OldScore],[NextPlayer,NewBoard,NewScore],_,0),
    %putEdge(Board, [0, 0], [Value1, Value2], NewBoard),
    NewBoard = NextBoard.

% Calculate Score
score([Value1,Value2],OldScore,NewScore,CurrentPlayer,NextPlayer) :- 
    	(   Value1 = 15, Value2 = 15 ),
    	NewScore is OldScore + 2,!,
    	NextPlayer = CurrentPlayer;
    	(   Value1 = 15; Value2 = 15 ), 
    	NewScore is OldScore + 1,!, 
    	NextPlayer = CurrentPlayer;
    	NewScore is OldScore, nextPlayer(CurrentPlayer,NextPlayer).

% Verify if it's finished
gameOver(UserScore, ComputerScore) :- 
    Total is UserScore + ComputerScore,
    Total = 9.

% Alternate Players
nextPlayer(user,computer).
nextPlayer(computer,user).

% Predicate move edge
% putEdge(Board, [Action, Index], [Value1, Value2], NewBoard) :- .


%(edge filled,value)
%for filling in edges, corresponds to value that needs to be added 
action(bottom,8).
action(top,2).
action(left,4).
action(right,1).

%(edge filled,index offset,value added to neighbour)
%Tells us who is the neighbour and how its value should be modified
neigh(bottom,3,2).
neigh(top,-3,8).
neigh(left,-1,1).
neigh(right,1,4).

%(edge filled,list of cases)
%Action with the corresponding special border cases
border(bottom,[6,7,8]).
border(top,[0,1,2]).
border(left,[0,3,6]).
border(right,[2,5,8]).

%for minimax:
%[Player,Board,OldScore] represents Pos
%[NextPlayer,NextBoard,NewScore] represents NextPos
move(Action,Index,[Player,Board,OldScore],[NextPlayer,NextBoard,NewScore]) :-
    checkMove(Action,Board,Index),
    moveaux(Action,Board,Index,[NewValue,ValueNeighbour],NextBoard),!,
    score([NewValue,ValueNeighbour],OldScore,NewScore,Player,NextPlayer).
    
%predicate that actually handles the "physical move"
moveaux(Action,Board,Index,[NewValue,ValueNeighbour],NewBoard) :-
    nth0(Index,Board,CurrentValue),
    CurrentValue < 15,
    action(Action,Bit),
    NewValue is CurrentValue + Bit,   
    replace(Board,Index,NewValue,TmpBoard),
	moveNeighbour(Action,TmpBoard,Index,ValueNeighbour,NewBoard).

%Predicate to handle to move of a neighbour box
moveNeighbour(Action,Board,Index,NewValue,NewBoard):-
    border(Action,Limit),
    neigh(Action,Offset,Bit),
    
	%use of if -> then; else
	%if our box is not a border case
    (  not(member(Index,Limit)) ->  (  NewIndex is Index+Offset,
                                     nth0(NewIndex,Board,CurrentValue),
                                     NewValue is CurrentValue+Bit,
                                     replace(Board,NewIndex,NewValue,NewBoard));
	%else do not change the board    
	NewBoard = Board,NewValue = 0  ). 

%To check if a move is legal(i.e. if the edge has already been drawn)
checkMove(Action,Board,Index):-
    nth0(Index,Board,CurrentValue),
    action(Action,Bit),
    check(CurrentValue,Bit),
    checkNeighbour(Action,Board,Index).

checkNeighbour(Action,Board,Index):-
    action(Action,Bit),
    border(Action,Limit),
    neigh(Action,Offset,Bit2),
    
    (  not(member(Index,Limit)) ->  (  NewIndex is Index+Offset,
                                     nth0(NewIndex,Board,CurrentValue),
                                     check(CurrentValue,Bit2));
	%else do not change the board    
	true ). 
    
    
check(CurrentValue,Bit):-
    Check is CurrentValue /\ Bit,
    not(Check is Bit).
    
    
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Include files
% Creation de la liste
createList([],_,0).
createList([X],X,1).
createList([X|L],X,N) :- plus(S,1,N), createList(L, X, S).

% Remplacer un element dans la liste
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% Affichage de la liste
displayBoard(L, UserScore, ComputerScore) :- 
    write('************* GAME ******************'), nl,
 	write('| '), nth0(0, L, E), write(E), write(' | '),
	nth0(1, L, E1), write(E1), write(' | '),
	nth0(2, L, E2), write(E2), write(' |'), nl,
    
	write('| '), nth0(3, L, E3), write(E3), write(' | '),
	nth0(4, L, E4), write(E4), write(' | '),
	nth0(5, L, E5), write(E5), write(' |'), nl,
    
	write('| '), nth0(6, L, E6), write(E6), write(' | '),
	nth0(7, L, E7), write(E7), write(' | '),
	nth0(8, L, E8), write(E8), write(' |'), nl,
    
    write('Your Score : '), write(UserScore), write(' | Computer Score : '),
    write(ComputerScore), nl.

% Predicate Display the winner 
displayWinner(UserScore,ComputerScore) :-
    % User is the winner
    UserScore > ComputerScore,
    write('You win :)');
    
    % Computer is the winner
    UserScore < ComputerScore,
    write('You lost :(');
    
    % Equality
    write('Replay :-').
