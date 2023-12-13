%main :- generator4(X), tester4(X), write(X).
main :- x_generator4(X), write(X).

% developer tried implementing the generate_list approach which is recursive, 
% however the answer can be computed faster with check_list approach which isn't recursive.

% testing with x_generator4 could rake a wrile, between (8-10) minutes because of all the probabilities, however, running the secret combination
% generator4(X), tester4(X) , write(X). will take approaximately 1 minuete on a slow computer.


% Generator 4 Test Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x_generator4( N ) :-
    x_generator4_loop(
    [ [[9 ,6 ,7] ,[4 ,0 ,1] ,[2 ,8 ,3] ,[5]]
    , [[9 ,8 ,3] ,[6 ,0 ,1] ,[5] ,[4 ,7] ,[2]]
    , [[9 ,8 ,3] ,[6 ,7] ,[4 ,2 ,0 ,1] ,[5]]
    , [[9 ,8 ,5 ,1] ,[2] ,[4 ,3] ,[6 ,0 ,7]]
    , [[9 ,8 ,5 ,1] ,[2] ,[3] ,[6 ,0 ,4 ,7]]
    , [[9 ,8 ,5 ,1] ,[2] ,[7] ,[4 ,6 ,0 ,3]]
    , [[8 ,9] ,[7] ,[6 ,0 ,1] ,[2 ,5 ,4 ,3]]
    , [[8 ,9] ,[7] ,[5 ,6 ,3] ,[4 ,0 ,2 ,1]]
    , [[8 ,9] ,[5] ,[4 ,7] ,[6 ,0 ,1] ,[3] ,[2]]
    , [[3],[5],[6,0,7],[2],[4,1],[8,9]] ], 0, N ).
    x_generator4_loop( [], C, C ). 
x_generator4_loop( [T|TS], C, N ) :-
    generator4( T ),
    C1 is C + 1,
    x_generator4_loop( TS, C1, N ). 
x_generator4_loop( [_|TS], C, N ) :-
    x_generator4_loop( TS, C, N ).


% Tester 4 Test Predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x_tester4( N ) :- 
    x_tester4_loop(
    [ [[8 ,2 ,7] ,[6 ,1] ,[5 ,3] ,[4 ,0 ,9]] 
    , [[8 ,2 ,7] ,[6 ,1] ,[4 ,0 ,9] ,[5 ,3]]
    , [[8 ,2 ,7] ,[5 ,3] ,[6 ,1] ,[4 ,0 ,9]]
    , [[8 ,2 ,7] ,[4 ,0 ,9] ,[6 ,1] ,[5 ,3]]
    , [[6 ,1] ,[8 ,2 ,7] ,[4 ,0 ,9] ,[5 ,3]]
    , [[6 ,1] ,[4 ,0 ,9] ,[5 ,3] ,[8 ,2 ,7]]
    , [[5 ,3] ,[6 ,1] ,[4 ,0 ,9] ,[8 ,2 ,7]]
    , [[5 ,3] ,[4 ,0 ,9] ,[6 ,1] ,[8 ,2 ,7]]
    , [[4 ,0 ,9] ,[5 ,3] ,[8 ,2 ,7] ,[6 ,1]]
    , [[4,0,9],[8,2,7],[6,1],[5,3]] ], 0, N ).
x_tester4_loop( [], C, C ). 
x_tester4_loop( [T|TS], C, N ) :-
    tester4( T ),
    C1 is C + 1,
    x_tester4_loop( TS, C1, N ).
x_tester4_loop( [_|TS], C, N ) :- 
    x_tester4_loop( TS, C, N ).


% Code For Cube Route
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generator4(X) :-
    per(M), check_list(M, S), permutation(S, X).

per(X) :-
    permutation([1,2,3,4,5,6,7,8,9,0], X).

generate_list([], []).
generate_list(L, X) :-
    generate_a_list(L, R, M), 
    first_not_zero(M),
    generate_list(R, X1),
    append([M], X1, X).

first_not_zero([H|_]) :-
    H =\= 0.

generate_a_list([H1, H2, H3, H4|T], T, [H1, H2, H3, H4]) :-
    length([H1, H2, H3, H4|T], N), N >= 4, 
    get_number([H1, H2, H3, H4], M), 
    prime(M).
generate_a_list([H1, H2, H3|T], T, [H1, H2, H3]) :-
    length([H1, H2, H3|T], N), N >= 3, \+ first_4_prime([H1, H2, H3|T]),
    get_number([H1, H2, H3], M), 
    prime(M).
generate_a_list([H1, H2|T], T, [H1, H2]) :-
    length([H1, H2|T], N), N >= 2, \+ first_4_prime([H1, H2|T]), \+ first_3_prime([H1, H2|T]),
    get_number([H1, H2], M), 
    prime(M).
generate_a_list([H1|T], T, [H1]) :-
    length([H1|T], N), N >= 1, \+ first_4_prime([H1|T]), \+ first_3_prime([H1|T]), \+ first_2_prime([H1|T]),
    get_number([H1], M), 
    prime(M).


check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H5 =\= 0, H9 =\= 0,
    get_number([H1, H2, H3, H4], M1), 
    prime(M1), get_number([H5, H6, H7, H8], M2), prime(M2), get_number([H9, H10], M3), prime(M3), X = [[H1, H2, H3, H4], [H5, H6, H7, H8], [H9, H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H5 =\= 0, H9 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3, H4], M1), 
    prime(M1), get_number([H5, H6, H7, H8], M2), prime(M2), prime(H9), prime(H10), X = [[H1, H2, H3, H4], [H5, H6, H7, H8], [H9], [H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H5 =\= 0, H8 =\= 0, 
    get_number([H1, H2, H3, H4], M1), 
    prime(M1), get_number([H5, H6, H7], M2), prime(M2), get_number([H8, H9, H10], M3), prime(M3), X = [[H1, H2, H3, H4], [H5, H6, H7], [H8, H9, H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H5 =\= 0, H8 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3, H4], M1), 
    prime(M1), get_number([H5, H6, H7], M2), prime(M2), get_number([H8, H9], M3), prime(M3), prime(H10), X = [[H1, H2, H3, H4], [H5, H6, H7], [H8, H9], [H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H5 =\= 0, H8 =\= 0, H9 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3, H4], M1), 
    prime(M1), get_number([H5, H6, H7], M2), prime(M2), prime(H8), prime(H9), prime(H10), X = [[H1, H2, H3, H4], [H5, H6, H7], [H8], [H9], [H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H5 =\= 0, H7 =\= 0, H9 =\= 0,
    get_number([H1, H2, H3, H4], M1), 
    prime(M1), get_number([H5, H6], M2), prime(M2), get_number([H7, H8], M3), prime(M3), get_number([H9, H10], M4), prime(M4), X = [[H1, H2, H3, H4], [H5, H6],  [H7, H8], [H9, H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H5 =\= 0, H7 =\= 0, H9 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3, H4], M1), 
    prime(M1), get_number([H5, H6], M2), prime(M2), get_number([H7, H8], M3), prime(M3), prime(H9), prime(H10), X = [[H1, H2, H3, H4], [H5, H6],  [H7, H8], [H9], [H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H5 =\= 0, H7 =\= 0, H8 =\= 0, H9 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3, H4], M1), 
    prime(M1), get_number([H5, H6], M2), prime(M2), prime(H7), prime(H8), prime(H9), prime(H10), X = [[H1, H2, H3, H4], [H5, H6], [H7], [H8], [H9], [H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H4 =\= 0, H7 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3], M1), 
    prime(M1), get_number([H4, H5, H6], M2), prime(M2), get_number([H7, H8, H9], M3), prime(M3), prime(H10), X = [[H1, H2, H3], [H4, H5, H6], [H7, H8 ,H9], [H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H4 =\= 0, H7 =\= 0, H9 =\= 0,
    get_number([H1, H2, H3], M1), 
    prime(M1), get_number([H4, H5, H6], M2), prime(M2), get_number([H7, H8], M3), prime(M3), get_number([H9, H10], M4), prime(M4), X = [[H1, H2, H3], [H4, H5, H6], [H7, H8] ,[H9 ,H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H4 =\= 0, H7 =\= 0, H9 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3], M1), 
    prime(M1), get_number([H4, H5, H6], M2), prime(M2), get_number([H7, H8], M3), prime(M3), prime(H9), prime(H10), X = [[H1, H2, H3], [H4, H5, H6], [H7, H8] ,[H9] ,[H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H4 =\= 0, H7 =\= 0, H8 =\= 0, H9 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3], M1), 
    prime(M1), get_number([H4, H5, H6], M2), prime(M2), prime(H7), prime(H8), prime(H9), prime(H10), X = [[H1, H2, H3], [H4, H5, H6], [H7], [H8] ,[H9] ,[H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H4 =\= 0, H6 =\= 0, H8 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3], M1), 
    prime(M1), get_number([H4, H5], M2), prime(M2), get_number([H6, H7], M3), prime(M3), get_number([H8, H9], M4), prime(M4), prime(H10), X = [[H1, H2, H3], [H4, H5], [H6,H7], [H8, H9] ,[H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H4 =\= 0, H6 =\= 0, H8 =\= 0, H9 =\= 0, H10 =\= 0,
    get_number([H1, H2, H3], M1), 
    prime(M1), get_number([H4, H5], M2), prime(M2), get_number([H6, H7], M3), prime(M3), prime(H8), prime(H9), prime(H10), X = [[H1, H2, H3], [H4, H5], [H6,H7], [H8], [H9] ,[H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H3 =\= 0, H5 =\= 0, H7 =\= 0, H9 =\= 0,
    get_number([H1, H2], M1), 
    prime(M1), get_number([H3, H4], M2), prime(M2), get_number([H5, H6], M3), prime(M3), get_number([H7, H8], M4), prime(M4), get_number([H9, H10], M5), prime(M5), X = [[H1, H2], [H3, H4], [H5,H6], [H7,H8] ,[H9, H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H3 =\= 0, H5 =\= 0, H7 =\= 0, H9 =\= 0, H10 =\= 0,
    get_number([H1, H2], M1), 
    prime(M1), get_number([H3, H4], M2), prime(M2), get_number([H5, H6], M3), prime(M3), get_number([H7, H8], M4), prime(M4), prime(H9), prime(H10), X = [[H1, H2], [H3, H4], [H5,H6], [H7,H8] ,[H9] ,[H10]].
check_list([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10], X) :-
    H1 =\= 0, H3 =\= 0, H5 =\= 0, H7 =\= 0, H8 =\= 0, H9 =\= 0, H10 =\= 0,
    get_number([H1, H2], M1), 
    prime(M1), get_number([H3, H4], M2), prime(M2), get_number([H5, H6], M3), prime(M3), prime(H7), prime(H8), prime(H9), prime(H10), X = [[H1, H2], [H3, H4], [H5,H6], [H7],[H8] ,[H9] ,[H10]].



different_lists(Lists) :-
    flatten(Lists, List),
    sort(List, Sorted),
    length(List, Len),
    length(Sorted, Len).

different([]).
different([H|T]) :-
    \+ member(H, T), different(T).

nested_length([], 0).
nested_length([H|T], N) :-
    length(H, L),
    nested_length(T, V),
    N is V + L.

list_length(A, N) :-
    flatten(A, AS),
    length(AS, N).

prime(X) :-
    X > 1,
    X1 is floor(sqrt(X)),
    \+ (between(2, X1, I), X mod I =:= 0).


numbers_to_list(N, [N]) :-
    N < 10.
numbers_to_list(N, X) :-
    N >= 10, 
    div_mod(N, 10, D, M), 
    numbers_to_list(D, R), 
    append(R, [M], X).

div_mod(A, B, D, M) :-
    D is A div B, 
    M is A mod B.

different_members([]).
different_members([H|T]) :-
    \+ member(H, T), different_members(T).


remove_the_prime(L, X) :-
    get_smallest_prime(L, S),
    remove_smallest_prime(L, S, X).

remove_smallest_prime([], _, []).
remove_smallest_prime([H|T], S, X) :-
    get_number(H, N),
    N =:= S,
    remove_smallest_prime(T, S, X).
remove_smallest_prime([H|T], S, [H|X]) :-
    get_number(H, N),
    N =\= S,
    remove_smallest_prime(T, S, X).


get_smallest_prime([H], X) :- get_number(H, X).
get_smallest_prime([H|T], N) :-
    get_number(H, N), 
    get_smallest_prime(T, N1), 
    N < N1.
get_smallest_prime([H|T], N1) :-
    get_number(H, N), 
    get_smallest_prime(T, N1), 
    N > N1.


get_number(XS, N) :-
    reverse_list(XS, SX), list_to_number(SX, N).

list_to_number([], 0).
list_to_number([H|T], N) :-
    list_to_number(T, N1), 
    N is N1 * 10 + H.


reverse_list([], []).
reverse_list([X|XS], W) :-
    reverse_list(XS, V), 
    append(V, [X], W).

is_cube(N) :-
    is_cube(N, 0).

is_cube(N, M) :-
    S is M * M * M,
    N =:= S.
is_cube(N, M) :-
    S is M * M * M,
    S < N,
    M1 is M + 1,
    is_cube(N, M1).


all_cube([]).
all_cube([H1, H2, H3, H4|T]) :-
    get_number([H1, H2, H3, H4], N),
    is_cube(N),
    all_cube(T).
all_cube([H1, H2, H3|T]) :-
    get_number([H1, H2, H3], N),
    is_cube(N),
    all_cube(T).
all_cube([H1, H2|T]) :-
    get_number([H1, H2], N),
    is_cube(N),
    all_cube(T).
all_cube([H|T]) :-
    is_cube(H), 
    all_cube(T).

insert_nested_list(E, [], [E]). 
insert_nested_list(E, [H|T], [E,H|T]) :-
    get_number(E, EN),
    get_number(H, HN),
    EN > HN.
insert_nested_list(E, [H|T], [H|W]) :- 	
    get_number(E, EN),
    get_number(H, HN),
    EN =< HN, insert_nested_list(E, T, W).

sort_nested_list([], []). 
sort_nested_list([H|T], X) :-
    sort_nested_list(T, W), insert_nested_list(H, W, X).

tester4(L) :-
    remove_the_prime(L, M),
    sort_nested_list(M, SM),
    flatten(SM, SFM), 
    all_cube(SFM).
    
