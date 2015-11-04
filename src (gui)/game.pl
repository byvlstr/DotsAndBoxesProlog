:-module(game,[play/11]).





% Verify if its finished
gameOver(UserScore, ComputerScore,Dim) :-
Total is UserScore + ComputerScore,
MaxScore is Dim*Dim,
Total = MaxScore.

% Alternate Players
nextPlayer(user,computer).
nextPlayer(computer,user).

% Prédicate of play/11
% +Board@ list: board game
% +UserScore@ integer: user score (Incremented by one)
% +ComputerScore@ integer: computer score (Incremented by one)
% +Player@ const: player turn [ user | computer ]
play(Action,Index,Board,NewBoard,User1Score,NewUser1Score,User2Score,NewUser2Score,Player,NextPlayer,Dim) :-

% User turn
Player = user,
userPlay(Action,Index,Board,NewBoard,User1Score,NewUser1Score,NextPlayer,Dim),
NewUser2Score is User2Score;

% Computer turn
Player = computer,
computerPlay(Board,NewBoard,User2Score,NewUser2Score,NextPlayer,Dim),
NewUser1Score is User1Score.

% Predicate user player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] its the next player turn
userPlay(Action, Index, Board,NewBoard,OldScore,NewScore,NextPlayer,Dim) :-
move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]).

% Predicate computer player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] its the next player turn
computerPlay(Board,NewBoard,OldScore,NewScore,NextPlayer,Dim) :-
ai(computer,Board,Action,Index,Dim),
move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]).


game(Dim) :-
Dimension is Dim*Dim,
createList(Board,0,Dimension),
play(Board,0,0,user,Dim).

