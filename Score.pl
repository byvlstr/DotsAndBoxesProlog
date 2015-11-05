%predicates to attribute score at each turn. They take in parameters:
%B the state of the board at the chosen turn
%P which player is playing
%P1 the score of player1
%P2 the score of player2
%P2X the new score of both players

score(Board, Player, ScoreP1, ScoreP2,NewScoreP1,NewScoreP2) :-    Player = user,                                        
                          check(B,ScoreP1,ScoreP2),                               
                          NewScoreP1 is ScoreP1+1,NewScoreP2 = ScoreP2.                                   
                                                                        
score(B, P, P1, P2,P21,P22) :-    Player = ,                                       
                          check(B,ScoreP1,ScoreP2),                               
                          NewScoreP2 is ScoreP1+1,NewScoreP1 = ScoreP1. 


%an intermediary predicate that compares the number of full boxes
%at the current turn, with the number of full boxes of the last turn
%if there is a positive difference between the two, we attribute a score

%Counts the number of full cases
countPts([], 0).
countPts([15|Q], R) :- countPts(Q, R1), !, R is R1+1.
countPts([_|Q], R) :- countPts(Q, R).

%A predicate that returns true if the difference of full boxes between
%the current turn and the sum of both scores is greater than zero
%T1 refers to the turn
%S1,S2 are the scores
%Z is the difference
check(T1,S1,S2) :- countPts(T1,X),Y is (S1+S2),Z is (X-Y),Z > 0.


