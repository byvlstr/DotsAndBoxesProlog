:-use_module(move).
:-use_module(display).
:-use_module(iARegle2).
:-['util.pl'].




% Verify if its finished
gameOver(UserScore, ComputerScore,Dim) :- 
    Total is UserScore + ComputerScore,
    MaxScore is Dim*Dim,
    Total = MaxScore.

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
		userPlay(Board,NewBoard,UserScore,NewUserScore,NextPlayer,Dim),
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

game(Dim) :-
    	Dimension is Dim*Dim,
        createList(Board,0,Dimension),
		play(Board,0,0,user,Dim).
