consult('mastermind.pl').

get_member_pos(2, [1,3,2,4], 3).

println('calc_guesses').
calc_guess([1,2,3,4], [1,2,3,4], 4, 0).
calc_guess([1,2,4,3], [1,2,3,4], 2, 2).
calc_guess([1,1,1,1], [1,2,3,4], 1, 0).
calc_guess([1,2,3,4], [4,3,2,1], 0, 4).
calc_guess([1,2,1,2], [1,2,3,4], 2, 0).

println('delete first occurence').
delete_first_occurence([1,2,3,4], 2, [1,3,4]).
delete_first_occurence([1,2,2,4], 2, [1,2,4]).
delete_first_occurence([1,2,3,4], 5, [1,2,3,4]).
delete_first_occurence([], 2, []).
