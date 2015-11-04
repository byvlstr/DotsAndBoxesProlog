:-module(display,[displayBoard/4,displayWinner/2]).

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
