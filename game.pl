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
	read(Index1), read(Index2),
    putEdge(Board, [Index1, Index2], [Value1,Value2], NewBoard),
    score([Value1,Value2],OldScore,NewScore,user,NextPlayer).

% Predicate computer player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] it's the next player turn
computerPlay(Board,NewBoard,OldScore,NewScore,NextPlayer) :- 
    write('Computer playing...'), nl, 
    % Here call for IA( Index1, Index2 ) algorithme :D
    putEdge(Board, [0, 0], [Value1, Value2], NewBoard),
    score([1,1],OldScore,NewScore,computer,NextPlayer).

% Calculate Score
score([Value1,Value2],OldScore,NewScore,CurrentPlayer,NextPlayer) :- 
    	(   Value1 = 15, Value2 = 15 ),
    	NewScore is OldScore + 2,
    	NextPlayer = CurrentPlayer;
    	(   Value1 = 15; Value2 = 15 ), 
    	NewScore is OldScore + 1, 
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
putEdge(Board, [Index1, Index2], [Value1, Value2], NewBoard) :-
    % Predicate convert here !!
    NewBoard = Board.
    
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
