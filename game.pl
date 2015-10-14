game :-
        init(Board),
    	displayList(Board),
    	readCell(C1,V1,C2,V2).


% Initialisation
init(Board) :- board(Board,0,9).

board([],_,0).
board([X],X,1).
board([X|L],X,N) :- board(L, X, S), plus(S,1,N).

% Affichage de la liste
displayList(L) :- write('************* GAME ******************\r\n'),
    			  write('| '), nth0(0, L, E), write(E), write(' | '),
    			  nth0(1, L, E), write(E), write(' | '),
    			  nth0(2, L, E), write(E), write(' |\r\n'),
    
    			  write('| '), nth0(3, L, E), write(E), write(' | '),
    			  nth0(4, L, E), write(E), write(' | '),
    			  nth0(5, L, E), write(E), write(' |\r\n'),
    
    			  write('| '), nth0(6, L, E), write(E), write(' | '),
    			  nth0(7, L, E), write(E), write(' | '),
    			  nth0(8, L, E), write(E), write(' |\r\n').
