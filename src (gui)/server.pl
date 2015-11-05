:- use_module(game).
:- use_module(move).
:- use_module(display).
:- use_module(iARegle2).
:- use_module(iARegle1).
:- use_module(iAMinimax).
:- ['iAregle3AlphaBeta.pl'].
:- ['util.pl'].
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).


stringTokenizer([], []).
stringTokenizer([N|Ns], [Digit|Rest]):-
atom_number(N, Digit),  stringTokenizer(Ns, Rest).

%PROLOG SERVER

%Server at http://localhost/game
:- http_handler(root(game), proceed_play,  []).
server(Port) :- http_server(http_dispatch, [port(Port)]).

%Handler for play requests from client
proceed_play(Request) :-
format('Access-Control-Allow-Origin: * ~n'),
format('Access-Control-Allow-Headers: origin, x-requested-with, content-type ~n'),
format('Access-Control-Allow-Methods: PUT, GET, POST, DELETE, OPTIONS ~n'),

http_parameters(Request,
%recuperation des parametres d URL
[
turnIndex(Index, [integer]),
turnAction(Action, []),
board(OldBoard, [string]),
user(User, []),
scoreUser1(ScoreUser1, [integer]),
scoreUser2(ScoreUser2, [integer]),
dim(Dim, [integer]),
mode(Mode, [integer]),
niveau(Niveau,[integer])
]),
format('Content-type: text/plain~n~n'),
split_string(OldBoard, ".", "", TmpBoard),
stringTokenizer(TmpBoard, Board),
play(Action,Index,Board,NewBoard,ScoreUser1,NewScoreUser1,ScoreUser2,NewScoreUser2,User,NextPlayer,Dim,Mode,Niveau),

%Send response in Json format
jsonState(NewBoard, NewScoreUser1, NewScoreUser2, NextPlayer, Dim).


%Predicat to send the Json response
jsonState([H|T], ScoreUser1, ScoreUser2, NextPlayer, Dim) :- format('{'),
jsonBoard([H|T]), jsonScoreUser1(ScoreUser1), jsonScoreUser2(ScoreUser2),
% On verifie si le jeu est termine
(gameOver(ScoreUser1, ScoreUser2, Dim), jsonGameOver(ScoreUser1, ScoreUser2), format('}'));

format('"game_over" : false, '), jsonPlayer(NextPlayer), format('}').


%Display the board using Json
jsonBoard([H|T]) :-
format('"board" : ['),
atomic_list_concat([H|T], ',', Atom),
format('~w,', Atom),
format('""], ').

%Display new score
jsonScoreUser1(X) :- format('"score_user1" : '), format('~d,', X).

%Display new score
jsonScoreUser2(X) :- format('"score_user2" : '), format('~d,', X).

%Display next player
jsonPlayer(X) :- format('"next_player" : "'), format('~w', X), format('"').

%Display results at the end of the game
jsonGameOver(ScoreUser1, ScoreUser2) :- format('"game_over" : true, "gagnant" :"'),
(ScoreUser1 > ScoreUser2,format('user"') );
(ScoreUser1 < ScoreUser2,format('computer"') );
(ScoreUser1 = ScoreUser2,format('aucun"') ).
