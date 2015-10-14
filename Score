score(B, R) :- countPts(B, R).
%TODO
%an intermediary predicate that compares the number of full boxes
%at the current turn, with the number of full boxes of the last turn
%if there is a positive difference between the two, we attribute a score

%Counts the number of full cases
countPts([], 0).
countPts([15|Q], R) :- countPts(Q, R1), !, R is R1+1.
countPts([_|Q], R) :- countPts(Q, R).
