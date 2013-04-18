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
% also add a list of all colors to the knowledge base
list_of_colors([red, orange, yellow, green, blue, violet]).

% color codes for initial guess
initial_code(1, _, [red]).
initial_code(2, _, [red,blue]).
initial_code(3, _, [red,blue,blue]).
initial_code(4, _, [red,red,blue,blue]).
initial_code(5, _, [red,red,blue,blue,blue]).

initial_code(1, random, [red]). 

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
guess_code(Solution_Code) :-
	guess_code(Solution_Code, five_guess, _). 

%% guess_code(+Solution_Code, +Pick_Method, -Used_Attemps)
%
% It starts with a fixed first guess, since the five_guess algorithm predicts to
% start with a [a a b b] kind of list. After evaluating this initial guess, it
% starts to reduce the set of all possible answers until it gets the one 
% solution.
guess_code(Solution_Code,Pick_Method,Used_Attempts) :-
	length(Solution_Code, Code_Length),
	pick_method(Pick_Method),
	initial_code(Code_Length, Pick_Method, Initial_Code), 
	print('Picked initial code: '),println(Initial_Code),  
	blacks_and_whites(Initial_Code, Solution_Code, Blacks, Whites),!,  
	create_all_code_permutations(Code_Length, All_Code_Permutations, _),
	check_and_reduce(
        9, Blacks, Whites, Initial_Code, Solution_Code, Code_Length,
        All_Code_Permutations, Pick_Method, Used_Attempts).
 
%% check_and_reduce(
%      +Counter, +Blacks, +Whites, +Guessed_Code, +Solution_Code, +Code_Length
%      +Possible_Codes, +Selection_Method, +Used_Attemps)
%
% The first predicate checks whether the number of blacks equals the length
% of the solution. In that case the game is won.
% The second predicate reduces the set of all possible permutations by those
% which became impossible by the given guessed code.
check_and_reduce(
    Counter, Code_Length, _, _, _, Code_Length, _, _, Used_Attempts
) :-
	print('I win! Chances left: '),
	Used_Attempts is 10 - Counter,
	println(Counter),
	!. 
check_and_reduce(
    Counter, Blacks, Whites, Guessed_Code, Solution_Code, Code_Length,
    Possible_Codes, Pick_Method, Used_Attempts
) :-
	remove_impossible_codes(
	    Guessed_Code, Blacks, Whites, Possible_Codes, Possible_Codes_Left),
	pick_and_print(
	    Counter, Possible_Codes_Left, Solution_Code, Code_Length, 
	    Pick_Method, Used_Attempts), 
    !.

%% pick_and_print(
%      +Counter, +Possible_Codes, +Solution_Code, +Code_Length, 
%      +Pick_Method, +Used_Attempts)
%
% Picks a new Code_Guess according to the specified Pick_Method, evaluates the
% number of blacks and whites and reduced all possible codes by those which
% get impossible through the new guess.
pick_and_print(
    Counter, Possible_Codes, Solution_Code, Code_Length, 
    Pick_Method, Used_Attempts
) :-
    Counter > 0,
    pick(Possible_Codes, Code_Length, Pick_Method, Code_Guess),
    print('Picked: '), println(Code_Guess), 
    blacks_and_whites(Code_Guess, Solution_Code, Blacks, Whites),
    length(Solution_Code, Code_Length), 
    Counter1 is Counter - 1,  
    check_and_reduce(
        Counter1, Blacks, Whites, Code_Guess, Solution_Code, Code_Length,
        Possible_Codes, Pick_Method, Used_Attempts).

%% pick(+Possible_Codes, +Code_Length, +Pick_Method, -Code_Guess)
%
% Picks a new Code_Guess according to the specified Pick_Method.
pick(Possible_Codes, Code_Length, five_guess, Code_Guess) :-
	master_pick(Code_Guess, Possible_Codes, Code_Length).
pick(Possible_Codes, _, random, Code_Guess) :-
	pick_random(Possible_Codes, Code_Guess).
	
%% blacks_and_whites(+Code_Guess, +Code_Answer, ?Blacks, ?Whites)
%
% Evaluates the Code_Guess against the Code_Answer (which need not be the same
% as the Code_Solution, e.g. when removing impossible codes from the 
% Possible_Codes) and either gives the number of Blacks and Whites or
% returns true, if they are specified and correct.
blacks_and_whites(Code_Guess, Code_Answer, Blacks, Whites) :-
	calc_black(Code_Guess, Code_Answer, Blacks),
	calc_white(Code_Guess, Code_Answer, Help),
	Whites is Help - Blacks.

%% calc_black(+Code_Guess, +Code_Answer, ?Blacks)
%
% Calculates the number of black pins, i.e. the number of colors which are
% at the correct position
calc_black([], [], Blacks) :- 
	Blacks is 0.
calc_black([Code_Guess_H|Code_Guess_R], [Code_Guess_H|Code_Answer_R], Blacks) :-
	calc_black(Code_Guess_R, Code_Answer_R, Help), 
	Blacks is 1 + Help.
calc_black([A|Code_Guess_R], [B|Code_Answer_R], Blacks) :-
	A \== B, 
	calc_black(Code_Guess_R, Code_Answer_R, Blacks).

%% calc_white(+Code_Guess, +Code_Answer, ?Whites_And_Blacks)
%
% Calculates the number of white pins (i.e. the number of colors which are in
% the answer, but not at the correct position) + the number of black pins
calc_white([], _, 0).
calc_white([Code_Guess_H|Code_Guess_R], Code_Answer, Whites_And_Blacks) :- 
	color(Code_Guess_H),
	member(Code_Guess_H, Code_Answer), 
	select(Code_Guess_H, Code_Answer, Answer_Without_Guess_H),
	!,
	calc_white(Code_Guess_R, Answer_Without_Guess_H, Help),!, 
	Whites_And_Blacks is 1 + Help.
calc_white([Code_Guess_H|Code_Guess_R], Code_Answer, Whites_And_Blacks) :- 
	color(Code_Guess_H),
	calc_white(Code_Guess_R, Code_Answer, Whites_And_Blacks).

%% remove_impossible_codes(
%      +Code_Guess, +Blacks, +Whites, +Possible_Codes, -Possible_Codes_Left)
%
% Removes all codes which become impossible through the Code_Guess with the 
% given number of Blacks and Whites
remove_impossible_codes(_,_,_,[],[]).
remove_impossible_codes(
    Code_Guess, Blacks, Whites, [Possible_Codes_H|Possible_Codes_R], 
    [Possible_Codes_H|Poss_Codes_Left_R]
) :- 
	blacks_and_whites(Code_Guess, Possible_Codes_H, Blacks, Whites), 
	remove_impossible_codes(
	    Code_Guess, Blacks, Whites, Possible_Codes_R, Poss_Codes_Left_R).
remove_impossible_codes(
    Code_Guess, Blacks, Whites, [_|Possible_Codes_R], Poss_Codes_Left):-
	remove_impossible_codes(
	   Code_Guess, Blacks, Whites, Possible_Codes_R, Poss_Codes_Left).

%% pick_random(+Possible_Codes, -Chosen_Code)
%
% First selection mode. Randomly chooses a code from the Possible_Codes.
pick_random(Possible_Codes, Chosen_Code) :-
	length(Possible_Codes, Length),
	%random_between(1, Length, Index) does not work with linux swi prolog
	random(1, Length + 1, Index),
	% get the Index'th element from Possible_Codes
	nth1(Index, Possible_Codes, Chosen_Code).

%% master_pick(-Code_Guess, +Possible_Codes, +Code_Length)
%
% Second selection mode. Picks a code from Possible_Codes with the five-guess 
% algorithm which works with the minimax strategy.
master_pick(Code_Guess, [Code_Guess|[]], _).
master_pick(Code_Guess, Possible_Codes, Code_Length) :-
	create_all_code_permutations(
	    Code_Length, All_Code_Permutations, Black_White_Permutations),
	length(Possible_Codes, Possible_Codes_Size),
	score_all_codes(
	    All_Code_Permutations, Possible_Codes, Possible_Codes_Size,
	    Black_White_Permutations, Score),
	pick_best(Score, Code_Guess), 
    !.

%% create_all_code_permutations(
%      +Code_Length, -All_Code_Permutations, -Black_White_Permutations)
%
% Creates a list of all possible permutations of colors as well as a list of
% all possible black and white permutations with the given length.
create_all_code_permutations(
    Code_Length, All_Code_Permutations, Black_White_Permutations
) :-
	list_of_colors(Color_List),	
	findall(
	   X, permutate_with_repetition(Color_List, Code_Length, X), 
	   All_Code_Permutations),
	findall(X, whites_blacks_relation(X, Code_Length), Black_White_Permutations).

%% permutate_with_repetition(+Items, +Number_of_Items, -Permutated_List)
%
% creates one permutation of all items including permutations with
% the same item multiple times.
permutate_with_repetition(Items, Number_of_Items, Permutated_List) :- 
	length(Permutated_List, Number_of_Items),
	permutate_h(Permutated_List, Items).
permutate_h([], _).
permutate_h([Item|Permutated_List_R], Item_List):-
	member(Item, Item_List),
	permutate_h(Permutated_List_R, Item_List).

%% score_all_codes(
%      +All_Code_Permutations, +Possible_Codes, +Possible_Codes_Size,
%      +Black_White_Permutations, -Scores)
%
% Calculates the score for every code. That is the minimum number of codes 
% which get eliminated when picking a code and assuming every 
% black/white combination once.
% In other words: 
% The score says how many possibilities get eliminated at least for each code.
score_all_codes([], _, _, _, []).
score_all_codes(
    [All_Code_Permutations_H|All_Code_Permutations_R], Possible_Codes, 
    Possible_Codes_Size, Black_White_Permutations, 
    [[All_Code_Permutations_H, Score]|Scores]
) :-
	findall(
	   Tmp_Score, (
	       member(Black_White_Permutation, Black_White_Permutations),
	       score_possibility(
	           All_Code_Permutations_H, Possible_Codes, Possible_Codes_Size,
	           Black_White_Permutation, Tmp_Score)), 
	   Tmp_Scores),
	% Chooses the minimal result from all black/white tries
	% --> Length - Tmp_Score = number of codes to be removed at least
	min_list(Tmp_Scores, Score),
	score_all_codes(
	    All_Code_Permutations_R, Possible_Codes, Possible_Codes_Size,
	    Black_White_Permutations, Scores). 

%% score_possibility(
%      +Code, +Possible_Codes, +Possible_Codes_Size
%      +Black_White_Permutation, -Score)
%
% Scores one Code with one Black_White_Permutation, i.e. the number of code
% which would be eliminated, when actually choosing this code and getting this
% Black_White_Permutation.
score_possibility(
    Code, Possible_Codes, Possible_Codes_Size, [Blacks, Whites], Score
) :-
	findall(
	   Possible_Code, (
	       member(Possible_Code, Possible_Codes),
	       blacks_and_whites(Code, Possible_Code, Blacks, Whites)),
	   Possible_Codes_Left),
	length(Possible_Codes_Left, Length),
	Score is Possible_Codes_Size - Length.

%% whites_blacks_relation(-Black_White_Permutation, +Code_Length)
%
% A predicate to create all possible black/white permutations.
whites_blacks_relation([Blacks, Whites], Code_Length) :-
	Blacks #>= 0, 
	Whites #>= 0, 
	Blacks + Whites #=< Code_Length,	
	% label([Blacks,Whites]) does not work with linux swi prolog
	labeling([], [Blacks, Whites]),
	% Rule to not produce combinations like 
	% [Blacks=3, Whites=1, Code_Length=4], [Blacks=4, Whites=1, Code_Length=5]
	% because they are impossible.
	not((Blacks =:= Code_Length - 1, Whites =:= 1)).

%% pick_best(+Scores, -Code_Guess)
%
% Picks the code with the highest score.
pick_best(Scores, Code_Guess) :- 
    pb_h(Scores, [[],0], Code_Guess).
pb_h([], [Code_Guess, Score], Code_Guess) :-
    printf('I pick: '), write(Code_Guess), 
    printf(' it eliminates at least '),  write(Score), 
    println(' possibilities').
pb_h([[Guess, Score]|Scores_R], [_, BestScore], Code_Guess) :- 
    Score >= BestScore, 
    pb_h(Scores_R, [Guess, Score], Code_Guess).
pb_h([[_, Score]|Scores_R], [BestGuess, BestScore], Code_Guess) :- 
    Score < BestScore, 
    pb_h(Scores_R, [BestGuess, BestScore], Code_Guess).

%% benchmark(+Code_Length, +Pick_Method)
%
% A benchmarks which tests every permutation of the given Code_Length with the
% specified Pick_Method (random or five_guess).
benchmark(Code_Length, Pick_Method) :-
	create_all_code_permutations(Code_Length, Codes, _),
	ben_h(Codes, Pick_Method, Sum_Codes, Min_Codes, Max_Codes, Counter),
	length(Codes, Number_Of_Codes),
	Fails is Number_Of_Codes - Counter,
	nl,nl,
	println('BENCHMARKRESULTS: ' ),
	print('Pick_Method: ' ),   println(Pick_Method),
	print('Length: ' ),        println(Code_Length),
	print('Possibilities: ' ), println(Number_Of_Codes),
	print('Succesfull: ' ),    println(Counter),
	print('Failed: ' ),        println(Fails),
	print('Min Attempts: ' ),  println(Min_Codes),
	print('Max Attempts: ' ),  println(Max_Codes),
	print('Total Attempts: ' ),println(Sum_Codes),
	!.
ben_h([], _, 0, 9999, 0, 0).
ben_h([Code|Codes], Pick_Method, Sum, Min, Max, Counter) :-
	guess_code(Code, Pick_Method, Attempts),
	ben_h(Codes, Pick_Method, Sum_Codes, Min_Codes, Max_Codes, Cnt),
	min(Attempts, Min_Codes, Min),
	max(Attempts, Max_Codes, Max),
	Sum is Attempts + Sum_Codes,
	Counter is Cnt + 1.

min(A,B,A) :- A < B.	
min(A,B,B) :- A >= B.

max(A,B,B) :- A =< B.	
max(A,B,A) :- A > B.
