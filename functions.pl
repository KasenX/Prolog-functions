% factorial(+N, -Result)
my_fact(0, 1).
my_fact(N, Res) :-
    N1 is N - 1,
    my_fact(N1, SubRes),
    Res is N * SubRes.

% fibonnaci(+N, -Result)
my_fibb(0, 0).
my_fibb(1, 1).
my_fibb(N, Res) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    my_fibb(N1, SubRes1),
    my_fibb(N2, SubRes2),
    Res is SubRes1 + SubRes2.

% contains(+List, +Element), check if the list contains the element
my_contains([E | _], E).
my_contains([_ | T], E) :-
    my_contains(T, E).

% not_contains(+Lst, +Elem), check if the list does not contain the element
my_not_contains([], _).
my_not_contains([H | T], E) :-
    H \= E,
    my_not_contains(T, E).

% nth(+List, +N, -Result), returns the n-th element in the list
my_nth([H | _], 1, H).
my_nth([_ | T], N, Res) :-
    N1 is N - 1,
    my_nth(T, N1, Res).

% length(+List, -Result), returns the length of the list
my_length([], 0).
my_length([_ | T], Res) :-
    my_length(T, SubRes),
    Res is SubRes + 1.

% lengthWithSublists(+List, -Result), return the length of the list including the lengths of the sublists
my_lengthWithSublists([], 0).
my_lengthWithSublists([H | T], Res) :-
    is_list(H),
    my_lengthWithSublists(H, SubRes1),
    my_lengthWithSublists(T, SubRes2),
    Res is SubRes1 + SubRes2, !.
my_lengthWithSublists([_ | T], Res) :-
    my_lengthWithSublists(T, SubRes),
    Res is SubRes + 1.

% append(+List, +Element, -Result), appends the element to the end of the list
my_append([], E, [E]).
my_append([H | T], E, [H | Res]) :-
    my_append(T, E, Res).

% prepend(+List, +Elemenet, -Result), prepends the element to the top of the list
my_prepend(Lst, E, [E | Lst]).

% delete_first(+List, +Elemenet, -Result), removes the first occurence of the element in the list
my_delete_first([E | T], E, T) :- !.
my_delete_first([H | T], E, [H |Res]) :-
    my_delete_first(T, E, Res).

% delete_all(+List, +Element, -Result), removes all occurences of the elements in the list
my_delete_all([], _, []).
my_delete_all([E | T], E, Res) :-
    my_delete_all(T, E, Res).
my_delete_all([H | T], E, [H|Res]) :-
    H \= E,
    my_delete_all(T, E, Res).

% delete_last(+List, +Elemenet, -Result), removes the last occurence of the element in the list


% replace(+List, +S, +R, -Result), replaces all occurences of S with R
my_replace([], _, _, []).
my_replace([R | T], R, S, [S | Res]) :-
    my_replace(T, R, S, Res), !.
my_replace([H | T], R, S, [H | Res]) :-
    my_replace(T, R, S, Res).

% last(+List, -Result), returns the last element of the list
my_last([E], E).
my_last([_ | T], Res) :-
    my_last(T, Res).

% count(+List, +Element, -Res), returns the count of occurences of element in the list
my_count([], _, 0).
my_count([E | T], E, Res) :-
    my_count(T, E, SubRes),
    Res is SubRes + 1, !.
my_count([_ | T], E, Res) :-
    my_count(T, E, Res).

% flatten(+List, -Result), flattens the list with its sublists
my_flatten([], []).
my_flatten([H | T], Res) :-
    is_list(H),
    my_flatten(H, FH),
    my_flatten(T, FT),  
    my_list_append(FH, FT, Res), !.
my_flatten([H | T], [H | Res]) :-
    my_flatten(T, Res).

% append(+List1, +List2, -Result), appends two lists
my_list_append([], Lst, Lst).
my_list_append([H | T], Lst, [H | Res]) :-
    my_list_append(T, Lst, Res).

% reverse(+List, -Result), revers the list
my_reverse(Lst, Reversed) :-
    my_reverse(Lst, [], Reversed).

my_reverse([], Acc, Acc).
my_reverse([H | T], Acc, Reversed) :-
    my_reverse(T, [H | Acc], Reversed).

% insert(+List, +Element, -Result), inserts the element in the sorted list (in the correct position)
my_insert([], E, [E]).
my_insert([H | T], E, [E ,H | T]) :-
    E < H, !.
my_insert([H | T], E, [H | Res]) :-
    E >= H,
    my_insert(T, E, Res).

% insert_sort(+List, -Result), implementation of insert sort
my_insert_sort(Lst, Res) :-
    my_insert_sort(Lst, [], Res).

my_insert_sort([], Sorted, Sorted).
my_insert_sort([H | T], Sorted, Res) :-
    my_insert(Sorted, H, SortedWithH),
    my_insert_sort(T, SortedWithH, Res).

% find_min(+List, -Result), returns the minimum from the list
my_find_min([Min], Min).
my_find_min([H, K | T], Min) :-
    H < K,
    my_find_min([H | T], Min), !.
my_find_min([_, K | T], Min) :-
    my_find_min([K | T], Min).

% select_sort(+List, -Result), implementation of select sort
my_select_sort(Lst, Res) :-
    my_select_sort(Lst, [], Res).

my_select_sort([], Sorted, Sorted).
my_select_sort(Unsorted, Sorted, Res) :-
    my_find_min(Unsorted, Min),
    my_delete_first(Unsorted, Min, UnsortedWithoutMin),
    my_append(Sorted, Min, SortedWithMin),
    my_select_sort(UnsortedWithoutMin, SortedWithMin, Res).

% select(+List, +Pivot, -Lesser, -Equal, -Greater), divides list into three lists with elements lesser than the pivot, equal to the pivot and greater than the pivot
my_select([], _, [], [], []).
my_select([H | T], Pivot, [H | Ls], Eq, Gt) :-
    H < Pivot,
    my_select(T, Pivot, Ls, Eq, Gt).
my_select([H | T], Pivot, Ls, [H | Eq], Gt) :-
    H = Pivot,
    my_select(T, Pivot, Ls, Eq, Gt).
my_select([H | T], Pivot, Ls, Eq, [H | Gt]) :-
    H > Pivot,
    my_select(T, Pivot, Ls, Eq, Gt).

% quick_sort(+List, -Result), implementation of quick sort
my_quick_sort([], []).
my_quick_sort([Pivot | T], Sorted) :-
    my_select([Pivot | T], Pivot, Ls, Eq, Gt),
    my_quick_sort(Ls, SortedLs),
    my_quick_sort(Gt, SortedGt),
    my_list_append(SortedLs, Eq, Tmp),
    my_list_append(Tmp, SortedGt, Sorted).

% split(+List, -Result1, -Result2), halve given list into two lists
my_halve(Lst, R1, R2) :-
    my_length(Lst, LstLen),
    R2Len is LstLen // 2,
    R1Len is LstLen - R2Len,
    my_length(R1, R1Len),
    my_length(R2, R2Len),
    !,
    my_list_append(R1, R2, Lst). 

% merge(+List1, +List2, -Result), merge two sorted list into one sorted list
my_merge(L1, [], L1) :- !.
my_merge([], L2, L2) :- !.
my_merge([H1 | T1], [H2 | T2], [H1 | Res]) :-
    H1 < H2,
    my_merge(T1, [H2 | T2], Res), !.
my_merge([H1 | T1], [H2 | T2], [H2 | Res]) :-
    H1 >= H2,
    my_merge([H1 | T1], T2, Res).


% merge_sort(+List, -Result), implementation of merge sort
my_merge_sort([], []) :- !.
my_merge_sort([A], [A]) :- !.
my_merge_sort(Unsorted, Sorted) :-
    my_halve(Unsorted, Lst1, Lst2),
    my_merge_sort(Lst1, SortedLst1),
    my_merge_sort(Lst2, SortedLst2),
    my_merge(SortedLst1, SortedLst2, Sorted).