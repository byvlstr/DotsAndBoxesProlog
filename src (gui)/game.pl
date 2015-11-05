:-module(game,[play/13,gameOver/3]).





% Verify if the game finished
gameOver(UserScore, ComputerScore,Dim) :-
Total is UserScore + ComputerScore,
MaxScore is Dim*Dim,
Total = MaxScore.

% Alternate Players
nextPlayer(user,computer).
nextPlayer(computer,user).

% Prédicate of play/13
% Our main predicate to play the game
% +Board@ list: board game
% +UserScore@ integer: user score (Incremented by one)
% +ComputerScore@ integer: computer score (Incremented by one)
% +Player@ const: player turn [ user | computer ]
% +Mode: 2 - User vs User, 3 - AI vs AI
% +Dim: dimensions of our square
% +Nieau: choice of AI
% +Action: action the player takes (i.e. botton)
% +Index: where the player takes the action
play(Action,Index,Board,NewBoard,User1Score,NewUser1Score,User2Score,NewUser2Score,Player,NextPlayer,Dim,Mode,Niveau) :-

% User turn
Player = user,
(Mode = 3, computerPlay2(Board,NewBoard,User2Score,User1Score,NewUser1Score,NextPlayer,Dim,Niveau), NewUser2Score is User2Score);

Player = user,
userPlay(Action,Index,Board,NewBoard,User1Score,NewUser1Score,NextPlayer,Dim),
NewUser2Score is User2Score;

% Computer turn
Player = computer,
(Mode = 2, userPlay2(Action,Index,Board,NewBoard,User2Score,NewUser2Score,NextPlayer,Dim), NewUser1Score is User1Score);

Player = computer,
computerPlay(Board,NewBoard,User1Score,User2Score,NewUser2Score,NextPlayer,Dim,Niveau),
NewUser1Score is User1Score.

% Predicate user player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% +Dim: dimension of our board
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] its the next player turn
userPlay(Action, Index, Board,NewBoard,OldScore,NewScore,NextPlayer,Dim) :-
move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]).

%Situations where we need a second human player
userPlay2(Action, Index, Board,NewBoard,OldScore,NewScore,NextPlayer,Dim) :-
move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]).



% Predicate computer player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] its the next player turn
computerPlay(Board,NewBoard,OtherScore,OldScore,NewScore,NextPlayer,Dim,Niveau) :-
(Niveau = 1, ai1(computer,Board,Action,Index,Dim)),
move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]);
(Niveau = 2, ai2(computer,Board,Action,Index,Dim)),
move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]);
(Niveau = 3, minimax([_,_,[computer,Board,OldScore]],[Action, Index,[_,_,_]],_,0,Dim)),
move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]);
(Niveau = 4, aiAlpha(computer,Board,Index,Action,Dim,[OtherScore,OldScore])),
move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]).

%for situations where we have another AI playing
computerPlay2(Board,NewBoard,OtherScore,OldScore,NewScore,NextPlayer,Dim,Niveau) :-
(Niveau = 1, ai1(computer,Board,Action,Index,Dim)),
move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]);
(Niveau = 2, ai2(computer,Board,Action,Index,Dim)),
move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]);
(Niveau = 3, minimax([_,_,[computer,Board,OldScore]],[Action, Index,[_,_,_]],_,0,Dim)),
move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]);
(Niveau = 4, aiAlpha(user,Board,Index,Action,Dim,[OtherScore,OldScore])),
move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]).


game(Dim) :-
Dimension is Dim*Dim,
createList(Board,0,Dimension),
play(Board,0,0,user,Dim).


