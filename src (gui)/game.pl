:-module(game,[play/13,gameOver/3]).





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
play(Action,Index,Board,NewBoard,User1Score,NewUser1Score,User2Score,NewUser2Score,Player,NextPlayer,Dim,Mode,Niveau) :-

% User turn
Player = user,
(Mode = 3, computerPlay2(Board,NewBoard,User1Score,NewUser1Score,NextPlayer,Dim,Niveau), NewUser2Score is User2Score);

Player = user,
userPlay(Action,Index,Board,NewBoard,User1Score,NewUser1Score,NextPlayer,Dim),
NewUser2Score is User2Score;

% Computer turn
Player = computer,
(Mode = 2, userPlay2(Action,Index,Board,NewBoard,User2Score,NewUser2Score,NextPlayer,Dim), NewUser1Score is User1Score);

Player = computer,
computerPlay(Board,NewBoard,User2Score,NewUser2Score,NextPlayer,Dim,Niveau),
NewUser1Score is User1Score.

% Predicate user player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] its the next player turn
userPlay(Action, Index, Board,NewBoard,OldScore,NewScore,NextPlayer,Dim) :-
move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]).

% Predicate user player2 /5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] its the next player turn
userPlay2(Action, Index, Board,NewBoard,OldScore,NewScore,NextPlayer,Dim) :-
move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]).



% Predicate computer player/5
% +Board@ list: board game
% +NewBoard@ list: the new board after playing
% +OldScore@ integer: the score before playing
% -NewScore@ integer: the score after playing
% -NextPlaying@ const: [ user | computer ] its the next player turn
computerPlay(Board,NewBoard,OldScore,NewScore,NextPlayer,Dim,Niveau) :-
(Niveau = 1, ai1(computer,Board,Action,Index,Dim)),
move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]);
(Niveau = 2, ai2(computer,Board,Action,Index,Dim)),
move(Action,Index,Dim,[computer,Board,OldScore],[NextPlayer,NewBoard,NewScore]).

computerPlay2(Board,NewBoard,OldScore,NewScore,NextPlayer,Dim,Niveau) :-
(Niveau = 1, ai1(computer,Board,Action,Index,Dim)),
move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]);
(Niveau = 2, ai2(computer,Board,Action,Index,Dim)),
move(Action,Index,Dim,[user,Board,OldScore],[NextPlayer,NewBoard,NewScore]).


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


game(Dim) :-
Dimension is Dim*Dim,
createList(Board,0,Dimension),
play(Board,0,0,user,Dim).


