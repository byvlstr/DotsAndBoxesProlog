% binary conversion facts
conv(0,[0,0,0,0]).
conv(1,[0,0,0,1]).
conv(2,[0,0,1,0]).
conv(3,[0,0,1,1]).
conv(4,[0,1,0,0]).
conv(5,[0,1,0,1]).
conv(6,[0,1,1,0]).
conv(7,[0,1,1,1]).
conv(8,[1,0,0,0]).
conv(9,[1,0,0,1]).
conv(10,[1,0,1,0]).
conv(11,[1,0,1,1]).
conv(12,[1,1,0,0]).
conv(13,[1,1,0,1]).
conv(14,[1,1,1,0]).
conv(15,[1,1,1,1]).
% binary conversion predicate
binconv(X,Y) :- conv(X,Y).

%move takes as arguments: board,index,value,binary value of B[C]
move(B, C, V, B1) :- nth0(C, B, X),
    				binconv(X, L),
    				legal(L, V1),
    				replace(1, L, V1, R),
    				V1 is 3-V,
    				binconv(R1,R),
    				replace(R1,B,C,B1). 

%check if a move is legal
%takes the index and the value of the edge
legal(C, V) :- nth0(V, C, X), X = 0.

%replace L1[N] by X, returns the result in L2
replace(X,[Z|L],0,[X|L]). 
replace(X,[Z|L1],N,[Z|L2]) :- replace(X,L1,N1,L2), N is N1+1.
