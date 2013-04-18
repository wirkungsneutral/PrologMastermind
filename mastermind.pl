/*
Mastermind - a prolog implementation of the 5-choice-algorithm as well as an 
implementation with random choice.

Filename: mastermind.pl
Version: 1.0.0
Authors: Tobias Schöneberger, Matthis Hauschild
*/
:- use_module(library(random)).
:- use_module(library(clpfd)).

% TODO kann das geloescht werden?
%http://www.cs.oswego.edu/~odendahl/coursework/notes/prolog/synopsis/con.html
%a([]) :- get_random_color(C),print_color(C).
%get_random_color(C):- random_between(1,6,C).
%test([]) : give_white([1,3,3,1],[1, 2, 2 ,2],).

println(A):-write(A),nl.
%print(A):-write(A).

% add supported colors to the knowledge base
color(red).
color(orange).
color(yellow).
color(green).
color(blue).
color(violet).
list_of_colors([red,orange,yellow,green,blue,violet]).

% color codes for initial guess
start_code(1,_,[red]).
start_code(2,_,[red,blue]).
start_code(3,_,[red,blue,blue]).
start_code(4,_,[red,red,blue,blue]).
start_code(5,_,[red,red,blue,blue,blue]).

start_code(1,random,[red]). 

% there are two supported methods:
%   random:     The selection of the next guess is random (very fast)
%   five_guess: The selection of the next guess is based on the five-guess
%               algorithm which is a minimax approach (can take a lot of time)
pick_method(random).
pick_method(five_guess).

%% guess_code(+Solution_Code)
%
% Main entry point for the program. Is a helperfunction, which calls the
% actual guess_code.
% It gets a colorcode as a list (e.g. [red, green, green, violet]) and prints
% each step to its solution. It is not allowed to use more than 10 steps.
% (But it will never need that many ;) )
guess_code(Solution_Code):-
	guess_code(Solution_Code,five_guess,_). 

%% guess_code(+Solution_Code, +Pick_Method, -Used_Attemps)
%
% It starts with a fixed first guess, since the five_guess algorithm predicts to
% start with a [a a b b] kind of list. After evaluating this initial guess, it
% starts to reduce the set of all possible answers until it gets the one 
% solution.
guess_code(Solution_Code,Pick_Method,Used_Attempts):-
	length(Solution_Code,Code_Length),
	pick_method(Pick_Method),
	start_code(Code_Length,Pick_Method,Start_Code), 
	print('Picked startcode: '),println(Start_Code),  
	black_and_white(Start_Code, Solution_Code, B, W),!,  
	fullSet(Code_Length, All_Possibilities, _),
	check_and_reduce(9,B,W,Start_Code,Solution_Code,Code_Length,All_Possibilities,Pick_Method,Used_Attempts)
.

%TODO
pick_and_print(Counter,Possible_Codes,Solution_Code,Code_Length,Pick_Method,Used_Attempts):-
	Counter > 0,
	pick(Possible_Codes,Code_Length,Pick_Method,Guess),
	print('Picked: '),println(Guess), 
	black_and_white(Guess, Solution_Code, B, W),
	length(Solution_Code,Code_Length), 
	Counter1 is Counter -1,  
	check_and_reduce(Counter1,B,W,Guess,Solution_Code,Code_Length,Possible_Codes,Pick_Method,Used_Attempts)
.
 
%% check_and_reduce(
%      +Counter, +Blacks, +Whites, +Guess, +Solution_Code, +Code_Length
%      +Possible_Codes, +Selection_Method, +Used_Attemps)
%
%   The first predicate checks whether the number of blacks equals the length
%   of the solution. In that case the game is won.
%   The second predicate reduces the set of all possible permutations by those
%   which became impossible by the given guess.
check_and_reduce(Counter,Laenge,_,_,_,Laenge,_,_,Used_Attempts) :-
	print('I win! Chances left: '),
	Used_Attempts is 10 - Counter,
	println(Counter),!. 
 
check_and_reduce(Counter,B,W,Guess,Solution_Code,Code_Length,Possible_Codes,Pick_Method,Used_Attempts):-
	remove_impossible(Guess,B,W,Possible_Codes,NewPossLeft),
	%findall(X,(member(X,Possible_Codes),black_and_white(Guess,X,B,W)),NewPossLeft),
	pick_and_print(Counter,NewPossLeft,Solution_Code,Code_Length,Pick_Method,Used_Attempts), 
!.


pick(Possible_Codes,Code_Length,five_guess,Guess):-
	master_pick(Guess,Possible_Codes,Code_Length).
	
pick(Possible_Codes,_,random,Guess):-
	pick_random(Possible_Codes,Guess).
	
	
% Berechnet die weissen und schwarzen Pins
% +Guess: 
% +Answer:  
% -Blacks: 
% -Whites:
%black_and_white(Guess, Answer, Blacks, _) :-
%	calc_black(Guess, Answer, Blacks),
%	Blacks == 4,
%	println('Gewonnen'), !.

black_and_white(Guess, Answer, Blacks, Whites) :-
	calc_black(Guess, Answer, Blacks),
	calc_white(Guess, Answer, Help),
	Whites is Help - Blacks.

% Guess List, Answer List, Number of Black Pins
calc_black([], [], Blacks) :- 
	Blacks is 0.

calc_black([GH|GR], [GH|AR], Blacks) :-
	calc_black(GR, AR, X), 
	Blacks is 1 + X.
		
calc_black([A|GR], [B|AR], Blacks) :-
	A \== B, 
	calc_black(GR, AR, Blacks).


% +Guess List, +Answer List, -Number of White Pins
calc_white([], _, 0).

calc_white([GH|GR], A, Whites) :- 
	color(GH),
	member(GH, A), 
	select(GH,A,AwithoutGH),!,
	calc_white(GR, AwithoutGH, X),!, 
	Whites is 1 + X
.

calc_white([GH|GR], A, Whites) :- 
	color(GH),
	calc_white(GR, A, Whites).
	

remove_impossible(_,_,_,[],[]).
remove_impossible(Guess,Black,White,[PH|PT],[PH|NPT]):- 
	black_and_white(Guess,PH,Black,White), 
	remove_impossible(Guess,Black,White,PT,NPT)
.
remove_impossible(Guess,Black,White,[_|PT],NPT):-
	remove_impossible(Guess,Black,White,PT,NPT)
.

pick_random(List,Element):-
	length(List,ListL),
	random_between(1,ListL,Index),
	nth1(Index,List,Element)
.


perm_with_repetion(Items,Length,List):- 
	length(List,Length),
	perm_h(List,Items)
.
perm_h([],_).
perm_h([Item|List1],ListOfItems):-
	member(Item,ListOfItems),
	perm_h(List1,ListOfItems)
.



fullSet(Length,Possible,BW_Combos):-
	list_of_colors(C),	
	findall(X,perm_with_repetion(C,Length,X),Possible),
	findall(X,whites_blacks_relation(X,Length),BW_Combos)
.
master_pick(Guess,[Guess|[]],_).
master_pick(Guess,PossibleCombinations,Code_Length):-
	fullSet(Code_Length,Full_Set,BW_Combos),
	length(PossibleCombinations,AM),
	score_full_set(Full_Set,PossibleCombinations,AM,BW_Combos,Score),
	pick_best(Score,Guess), 
!.

score_full_set([],_,_,_,[]).
score_full_set([P|T],PossibleCombinations,AM,BW_Combos,[[P,S1]|Score]):-
	findall(S,(member(BW,BW_Combos),score_possibility(P,PossibleCombinations,AM,BW,S)),L),
	%Waehlt das "schlechteste" Ergebniss der Weis/schwarz versuche --> Length - S = Mindestanzahl der entfernten elemente
	min_list(L,S1),
	score_full_set(T,PossibleCombinations,AM,BW_Combos,Score)
. 
 

score_possibility(P,PossibleCombinations,AM,[B,W],S):-
	findall(X,(member(X,PossibleCombinations),black_and_white(P,X,B,W)),Liste_mit_noch_moeglichen),
	length(Liste_mit_noch_moeglichen,Laenge_Liste),
	S is AM-Laenge_Liste    
. 
 
whites_blacks_relation([B,W], Length):-
	B #>= 0, W #>=0, 
	B +  W #=< Length,	
	% does not work with linux swi prolog
	%label([B,W]), 
	labeling([], [B,W]),
	%Rule to not produce combinations like [B=3,W=1,Length=4],[B=4,W=1,Length=5]  because they are impossible
	not((B =:= Length-1, W =:=1)).
	

pick_best(Score,Guess):- pb_h(Score,[[],0],Guess).
pb_h([],[Guess,Score],Guess):- 	printf('I pick: '), write(Guess), printf(' it eliminates at least '),  write(Score), println(' possibilities').
pb_h([[Gue,Sco]|ST],[_,BestSco],Guess):- Sco >= BestSco, pb_h(ST,[Gue,Sco],Guess).
pb_h([[_,Sco]|ST],[BestGue,BestSco],Guess):- Sco < BestSco, pb_h(ST,[BestGue,BestSco],Guess).

benchmark(Code_Length,Pick_Method):-
	fullSet(Code_Length,Codes,_),
	ben_h(Codes,Pick_Method,Sum_Codes,Min_Codes,Max_Codes,Counter),
	length(Codes,Number_Of_Codes),
	Fails is Number_Of_Codes - Counter,
	nl,nl,
	println('BENCHMARKRESULTS: ' ),
	print('Pick_Method: ' ), println(Pick_Method),
	print('Length: ' ), println(Code_Length),
	print('Possibilities: ' ), println(Number_Of_Codes),
	print('Succesfull: ' ), println(Counter),
	print('Failed: ' ), println(Fails),
	print('Min Attempts: ' ), println(Min_Codes),
	print('Max Attempts: ' ), println(Max_Codes),
	print('Total Attempts: ' ), println(Sum_Codes),!
.

ben_h([],_,0,9999,0,0).

ben_h([Code|Codes],Pick_Method,Sum,Min,Max,Counter):-
	guess_code(Code,Pick_Method,Attempts),
	ben_h(Codes,Pick_Method,Sum_Codes,Min_Codes,Max_Codes,Cnt),
	min(Attempts,Min_Codes,Min),
	max(Attempts,Max_Codes,Max),
	Sum is Attempts + Sum_Codes,
	Counter is Cnt +1
.

min(A,B,A):- A < B.	
min(A,B,B):- A >= B.

max(A,B,B):- A =< B.	
max(A,B,A):- A > B.
