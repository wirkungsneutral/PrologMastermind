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


print_color(1):- write('green').
print_color(2):- write('yellow').
print_color(3):- write('red').
print_color(4):- write('blue').
print_color(5):- write('pink').
print_color(6):- write('orange').

print_try(A):-write('['),print_try_h(A).
print_try_h([]):- 
	println(']'),!.
print_try_h([AH|[]]):- 
	print_color(AH), 
	println(']'),!.
print_try_h([AH|AR]):- 
	print_color(AH),
	write(', '),
	print_try_h(AR),!
.	

fight(Answer,MaxAttempts):-
	init(Answer,FirstAttempt),  
	AttemptsLeft is MaxAttempts - 1, 
	fight_loop(Answer, FirstAttempt, AttemptsLeft)
. 

init(Answer,FirstAttempt):- 
	write('[init] Correct answer: '),
	print_try(Answer),
	write('[init] Length: '), 
	length(Answer,L),
	println(L),
	%gen_random(L,FirstAttempt1),
	FirstAttempt1 = [1,2],
	write('[init] Testing: '),
	print_try(FirstAttempt1),
	check_h(FirstAttempt1, Answer, B, W),
	write('[init] Black: '),write(B),write('  White: '),println(W),
	%append([FirstAttempt1],[B,W],FirstAttempt) 
	FirstAttempt = [[FirstAttempt1,B,W]],
	check_win(B),!
.

check_win(2):-
	println('Der Geraet gewinnt!'),
	abort.
check_win(_).
 
fight_loop(_,_,0):- 
	println('[fight_loop] I lost :('),
	!
.

fight_loop(Answer, AlreadyTried, AttemptsLeft) :- 
	write('[fight_loop] Attempts Left: '),
	writeln(AttemptsLeft), 
	Attempts is AttemptsLeft - 1,
	get_possible_answer(Guess,AlreadyTried),
	write('[fight_loop] I guess: '),
	print_try(Guess),
	calc_guess(Guess, Answer, B,W), 
	check_win(B),
	write('[fight_loop] Black: '),write(B),write('  White: '),println(W),
	append(AlreadyTried,[[Guess,B,W]], AlrdyTrd),
%	println(AlrdyTrd),
	fight_loop(Answer,AlrdyTrd,Attempts),
	! 
.


one_to_six(A):-
	A #>= 1,
	A #=< 6,
	label([A])
.


get_possible_answer(_,[]). %:-println('End of life').
get_possible_answer(FullGuess, [[PreviousTry,Blacks,Whites]|ATR]):-
%	println(PreviousTry),
%	println(ATR),
	%println(Guess), 
	Guess\==PreviousTry,
	%println(Guess), 
	calc_guess(Guess,PreviousTry,Blacks,Whites),
	calc_guess(Guess,PreviousTry,Blacks,Whites),	
	%println(Guess), 
	get_possible_answer(Guess,ATR),
	%println(Guess),	
	add_random_colors(Guess,FullGuess) ,
	Guess\==PreviousTry
. 

add_random_colors([], []).
add_random_colors([GH|GR], Guess):-
	ground(GH),
	add_random_colors(GR, Guess1),
	append([GH], Guess1, Guess)
.
add_random_colors([GH|GR], Guess):-
	not(ground(GH)),
%	write('not'),
	C in 1..6,
	label([C]),
	add_random_colors(GR, Guess1),
	append([C], Guess1, Guess)
.

check_h(Guess,Answer,B,W):-
	check(Guess,Answer,Answer,[],W,B),!
.
check([],_,_,H,W,0):-length(H,W).
check([E|GR],[E|AT],A,H,W,B):-
	delete_first_occurence(H, E, H1),
	check(GR,AT,A,H1,W,B1),
	B is B1 + 1
.
check([GH|GR],[AH|AT],A,H,W,B):-
	GH\==AH,
	member(GH,A),
	delete_first_occurence(A, GH, A1),
	append([GH],H,H1),
	check(GR,AT,A1,H1,W,B)
.
check([GH|GR],[AH|AT],A,H,W,B):-
	GH\==AH,
	check(GR,AT,A,H,W,B)
.
	

gen_random(0,[]).
gen_random(Length, R):-
	L is Length -1,	
	gen_random(L,R1),
	get_random_color(C),
	append([C],R1,R)
.
 
%delete_first([],_,[]).
%delete_first([E|LR],E,LR).
%delete_first([LH|LR],E,R):- 	
%	LH \== E,
%	delete_first(LR,E,R1),
%	append([LH],R1,R).
	
nonmember(_,[]).	
nonmember(E,[LH|LR]):- E\==LH, nonmember(E,LR).

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
%	println(Guess),
%	println(Blacks),
	calc_white(Guess, Answer, Help),
%	println(Help),
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

color(red).
color(orange).
color(yellow).
color(green).
color(blue).
color(violet).	
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
	
	

%List to delete Element from, Element, New List
delete_first_occurence([], _, []).	
delete_first_occurence([Element|LR], Element, LR).
delete_first_occurence([LH|LR], Element, NewList) :-
	LH \== Element,
	delete_first_occurence(LR, Element, X),
	append([LH], X, NewList).



best_member(_,-1,_):-fail.
best_member(E,_,[E|_]).
best_member(E,C,[_|LT]):-
	C1 is C-1,
	best_member(E,C1,LT)
. 


tobi([],0).  
tobi([_|LT],C):- tobi(LT,C1), C is C1+1,!. 

white_and_black(L1,L2,L3):-
	length(L1,L),
	length(L2,L),
	quick_sort(L1,SL1),
	quick_sort(L2,Sl2),
	wab(SL1,Sl2,L3)
.
wab([],_,0).
wab(_,[],0).
wab([H1|T1],[H1|T2],Cnt):- Cnt1 is Cnt -1, wab(T1,T2,Cnt1).
wab([H1|T1],[H2|T2],Cnt):- H1 @> H2,wab(T1,[H2|T2],Cnt).
wab([H1|T1],[H2|T2],Cnt):- H1 @< H2,wab([H1|T1],T2,Cnt).


%SORT FROM: http://kti.mff.cuni.cz/~bartak/prolog/sorting.html
quick_sort(List,Sorted):-q_sort(List,[],Sorted).
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
    pivoting(H,T,L1,L2),
    q_sort(L1,Acc,Sorted1),q_sort(L2,[H|Sorted1],Sorted).
    
pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G):-X@=<H,pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-X@>H,pivoting(H,T,L,G).
 

 
 
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

pick(Element,List):-
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
:-dynamic code_length/1.

solve_code(Code):-
	Start_code=[red,red,blue,blue],
	check_h(start_code, Code, B, W),!,
	length(CL,Code),
	fullSet(CL, Possibilities, _),
	(B =:= CL) -> (print('I win'),
		true); 
		(
		findall(X,(member(X,Possibilities),check_h(Start_code,X,B,W)),NewPossLeft),
		try(9,NewPossLeft,Code)),
	!.
	
	
try(Counter,PossibilitiesLeft,Code):-
	Counter > 0,
	master_pick(Guess,PossibilitiesLeft),
	check_h(Guess, Code, B, W),
	length(CL,Code),
	(B =:= CL) -> (print('I win'),
		true); 
		(Counter1 = Counter -1,  
		findall(X,(member(X,PossibilitiesLeft),check_h(Guess,X,B,W)),NewPossLeft),
		try(Counter1,NewPossLeft,Code)),
	!.

fullSet(Length,Possible,BW_Combos):-
	C = [red,blue,green,blue,violet,orange],
	findall(X,perm_with_repetion(C,Length,X),Possible),
	findall(X,white_and_black_validate(X,Length),BW_Combos)
.
master_pick(Guess,PossibleCombinations,Length):-
	fullSet(Length,FullSet,BW_Combos),
	score_full_set(FullSet,PossibleCombinations,BW_Combos,Score),
	pick_best(Score,Guess),
!.

score_full_set([],_,_,[]).
score_full_set([P|T],PossibleCombinations,BW_Combos,[[P,S1]|Score]):-
	findall(S,(member(BW,BW_Combos),score_possibility(P,PossibleCombinations,BW,S)),L),
	%Waehlt das "schlechteste" Ergebniss der Weis/schwarz versuche --> Length - S = Mindestanzahl der entfernten elemente
	max_list(L,S1),
	score_full_set(T,PossibleCombinations,BW_Combos,Score)
.


score_possibility(P,PossibleCombinations,[B,W],S):-
	findall(X,(member(X,PossibleCombinations),check_h(P,X,B,W)),A),
	length(A,S)
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
	 