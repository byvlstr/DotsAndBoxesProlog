:- use_module(game).
:-use_module(move).
:-use_module(display).
:-use_module(iARegle1).
:-use_module(iAMinimax).
:-['util.pl'].
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uri)).


%PROLOG SERVER

%Mise en place du server à ladresse http://localhost/game
:- http_handler(root(game), proceed_play,  []).
server(Port) :- http_server(http_dispatch, [port(Port)]).

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
dim(Dim, [integer])
]),
format('Content-type: text/plain~n~n'),
split_string(OldBoard, ".", "", TmpBoard),
stringTokenizer(TmpBoard, Board),
play(Action,Index,Board,NewBoard,ScoreUser1,NewScoreUser1,ScoreUser2,NewScoreUser2,User,NextPlayer,Dim),

%On envoie les reponses par json
jsonState(NewBoard, NewScoreUser1, NewScoreUser2, NextPlayer).

%Container JSON
jsonState([H|T], ScoreUser1, ScoreUser2, NextPlayer) :- format('{'), jsonBoard([H|T]), jsonScoreUser1(ScoreUser1), jsonScoreUser2(ScoreUser2), jsonPlayer(NextPlayer), format('}').

%Affichage de la board en JSON
jsonBoard([H|T]) :-
format('"board" : ['),
atomic_list_concat([H|T], ',', Atom),
format('~w,', Atom),
format('""], ').

%Affichage du nouveau score
jsonScoreUser1(X) :- format('"score_user1" : '), format('~d,', X).

%Affichage du nouveau score
jsonScoreUser2(X) :- format('"score_user2" : '), format('~d,', X).

%Affichage du prochain joueur
jsonPlayer(X) :- format('"next_player" : "'), format('~w', X), format('"').

stringTokenizer([], []).
stringTokenizer([N|Ns], [Digit|Rest]):-
atom_number(N, Digit),  stringTokenizer(Ns, Rest).
