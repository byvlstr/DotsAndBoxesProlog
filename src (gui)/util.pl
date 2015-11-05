% Include files
% List Creation
createList([],_,0).
createList([X],X,1).
createList([X|L],X,N) :- plus(S,1,N), createList(L, X, S).

% Replace an element in a using its index
replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R), !.
replace(L, _, _, L).

% A replace that deals with two cases, easier for our game
% do not replace a value of -1
gameReplace(Value1,Value2,Index1,Index2,List,NewList):-
    (   replace(List,Index1,Value1,TmpList);TmpList = List),
    (   replace(TmpList,Index2,Value2,NewList);true),!.




