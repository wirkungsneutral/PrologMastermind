:- use_module(library(random)).
:- use_module(library(clpfd)).

%http://www.cs.oswego.edu/~odendahl/coursework/notes/prolog/synopsis/con.html
a([]) :- get_random_color(C),print_color(C).

println(A):-write(A),nl.
print(A):-write(A).
%test([]) : give_white([1,3,3,1],[1, 2, 2 ,2],).

get_random_color(C):- random_between(1,6,C).

%Fuer Windows
%give_random_color(C):- random_between(1,6,C).
%Fuer Linux, obwohl deprecated, aber random_between findet er nicht, wohl zu neu

%Farbdefinitionen
color(red).
color(orange).
color(yellow).
color(green).
color(blue).
color(violet).
list_of_colors([red,orange,yellow,green,blue,violet]).

%Methodiken
method(random).
method(five_guess).

guess_code(Code):-
	guess_code(Code,five_guess,_).

guess_code(Code,Methode,Used_Attempts):-
	Start_Code = [red,red,blue,blue], 
	print('Picked: '),println(Start_Code),  
	calc_guess(Start_Code, Code, B, W),!,  
	length(Code,CL),
	fullSet(CL, All_Possibilities, _),
	check_and_reduce(9,B,W,Start_Code,Code,CL,All_Possibilities,Methode,Used_Attempts)
.

pick_and_print(Counter,PossibilitiesLeft,Code,CL,Methode,Used_Attempts):-
	Counter > 0,
	pick(PossibilitiesLeft,CL,Methode,Guess),
	print('Picked: '),println(Guess), 
	calc_guess(Guess, Code, B, W),
	length(Code,CL), 
	Counter1 is Counter -1,  
	check_and_reduce(Counter1,B,W,Guess,Code,CL,PossibilitiesLeft,Methode,Used_Attempts)
.
 
check_and_reduce(Counter,Laenge,_,_,_,Laenge,_,_,Used_Attempts):-
	print('I win! Chances left: '),
	Used_Attempts is 10 - Counter,
	println(Counter),!. 
 
check_and_reduce(Counter1,B,W,Guess,Code,CL,PossibilitiesLeft,Methode,Used_Attempts):-
	%remove_impossible(Guess,B,W,PossibilitiesLeft,NewPossLeft),
	findall(X,(member(X,PossibilitiesLeft),calc_guess(Guess,X,B,W)),NewPossLeft),
	pick_and_print(Counter1,NewPossLeft,Code,CL,Methode,Used_Attempts), 
!.

pick(PossibilitiesLeft,CL,five_guess,Guess):-
	master_pick(Guess,PossibilitiesLeft,CL).
pick(PossibilitiesLeft,_,five_guess,Guess):-
	pick_random(PossibilitiesLeft,Guess).
% Berechnet die weissen und schwarzen Pins
% +Guess: 
% +Answer: 
% -Blacks: 
% -Whites:
%calc_guess(Guess, Answer, Blacks, _) :-
%	calc_black(Guess, Answer, Blacks),
%	Blacks == 4,
%	println('Gewonnen'), !.

calc_guess(Guess, Answer, Blacks, Whites) :-
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
	
	

just_win(Answer,Tries):-
	C = [red,blue,green,blue,violet,orange],
	length(Answer,Length),
	findall(X,perm_with_repetion(C,Length,X),Possible),
	win_h(Answer,Tries,Length,Possible)
.

win_h(_,0,_,_):-println('LOST'),!.
win_h(Answer,Tries,Length,Possible):-
	pick(Guess,Possible), 
		printf('Tries Left: '), 
		printf(Tries), 
		printf('  Trying: '), 
		println(Guess),
	check_h(Guess,Answer,Black,White),
		printf('#Black: '), 
		printf(Black), 
		printf('  #White: '), 
		println(White), 
	check_win(Guess,Black,Length),
	remove_impossible(Guess,Black,White,Possible,NewPossible),
		length(NewPossible,LenNP),
		printf('#Possibilities left: '), 
		println(LenNP),nl, 
	T1 is Tries -1,
	win_h(Answer,T1,Length,NewPossible)
.

remove_impossible(_,_,_,[],[]).
remove_impossible(Guess,Black,White,[PH|PT],[PH|NPT]):- 
	check_h(Guess,PH,Black,White), 
	remove_impossible(Guess,Black,White,PT,NPT)
.
remove_impossible(Guess,Black,White,[_|PT],NPT):-
	remove_impossible(Guess,Black,White,PT,NPT)
.
	
check_win(Guess,Blacks,Length):- 
	Blacks == Length , 
	println('WIN!'),
	println(Guess),abort.
check_win(_,_,_).

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
	findall(X,white_and_black_validate(X,Length),BW_Combos)
.
master_pick(Guess,[Guess|[]],_).
master_pick(Guess,PossibleCombinations,CL):-
	fullSet(CL,FullSet,BW_Combos),
	length(PossibleCombinations,AM),
	score_full_set(FullSet,PossibleCombinations,AM,BW_Combos,Score),
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
	findall(X,(member(X,PossibleCombinations),calc_guess(P,X,B,W)),Liste_mit_noch_moeglichen),
	length(Liste_mit_noch_moeglichen,Laenge_Liste),
	S is AM-Laenge_Liste    
. 
 
white_and_black_validate([B,W], Length):-
	B #>= 0, W #>=0, 
	B +  W #=< Length,	
	label([B,W]), 
	%Rule to not produce combinations like [B=3,W=1,Length=4],[B=4,W=1,Length=5]  because they are impossible
	not((B =:= Length-1, W =:=1)).
	

pick_best(Score,Guess):- pb_h(Score,[[],0],Guess).
pb_h([],[Guess,Score],Guess):- 	printf('I pick: '), write(Guess), printf(' it eliminates at least '),  write(Score), println(' possibilities').
pb_h([[Gue,Sco]|ST],[_,BestSco],Guess):- Sco >= BestSco, pb_h(ST,[Gue,Sco],Guess).
pb_h([[_,Sco]|ST],[BestGue,BestSco],Guess):- Sco < BestSco, pb_h(ST,[BestGue,BestSco],Guess).
	 