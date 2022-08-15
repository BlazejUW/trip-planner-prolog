% Blazej Palkus

:- ensure_loaded(library(lists)).
user:runtime_entry(start):-
    (current_prolog_flag(argv, [File]) ->
        set_prolog_flag(fileerrors, off),
        (compile(File) -> true
         ;
	     format('Error opening file ~p.\n', [File])
        ),
        prompt(_Old, ''),         % empty prompt
	    start
     ;
	    write('Incorrect usage, use: program <file>\n')
    ).

% ===============GENERATING TRIPS THAT MEET THE CONDITIONS================

generate_trip_meeting_conditions(Start, End, Tids, Length, Conditions) :-
    % add an empty list as VisitedRoutes, because in the first call there are no
    % visited routes yet
    % add 0 as a length accumulator
    generate_trip(Start, End, [], Tids, 0, Length, Conditions).

% recursively generates the next steps of the trip from Start to End
generate_trip(Start, End, VisitedRoutes,
    [(Tid,Start,Direction)|Tids], Acc, Length, Conditions) :-
    (
        % we search for a route starting at Start and ending at Next
        %(going right)
        route(Tid, Start, Next, _, _, Distance),
        Direction = toRight
    ;
        % if there is no such route, we search for a route starting at Next
        % and ending at Start but marked as "oba"
        %(meaning we can go left)
        route(Tid, Next, Start, _, oba, Distance),
        Direction = toLeft
    ),
    % if it returns true, it means we have not yet visited this route
    \+ member((Tid,Start,Direction), VisitedRoutes),
    route_meets_type_conditions(Tid, Conditions),
    (
        % if the found route ends at End, we stop generating
        End == Next,
        Length is Acc + Distance,
        check_length(Conditions, Length),
        Tids = []
    ;
        % if not, we generate the next steps of the trip
        NewAcc is Acc + Distance,
        NewVisitedRoutes = [(Tid, Start, Direction)|VisitedRoutes],
        (generate_trip(Next, End, NewVisitedRoutes, Tids,
            NewAcc, Length, Conditions))
    ).

%= ===============CHECKING IF THE ROUTE MEETS THE CONDITIONS================
route_meets_type_conditions(Tid, Conditions) :-
    % select the route with id Tid, retrieve its type and length
    route(Tid, _, _, Type, _, _),
    % check if the type is in the conditions list
    check_type(Conditions, Type).

check_type(Conditions, Type) :-
    check_number_of_types_in_data(Conditions, Number) ->
    (
        Number =:= 0
        ;
        member(type(Type), Conditions)
    ).

check_length(Conditions, Length) :-
    check_number_of_lengths_in_data(Conditions, Number) ->
    (
        Number =:= 0
        ;
        member(length(Op, Value), Conditions) ->
            check_operator(Op, Value, Length)
    ).

check_operator(eq, Value, Length) :- Length =:= Value.
check_operator(lt, Value, Length) :- Length < Value.
check_operator(le, Value, Length) :- Length =< Value.
check_operator(gt, Value, Length) :- Length > Value.
check_operator(ge, Value, Length) :- Length >= Value.

% how many different types are given in Conditions
check_number_of_types_in_data(Conditions, Number) :-
    include(is_type, Conditions, FoundTypes),
    length(FoundTypes, Number).

is_type(type(_)).

check_number_of_lengths_in_data(Conditions, Number) :-
    include(is_length, Conditions, Lengths),
    length(Lengths, Number).

is_length(length(_, _)).

% ===============PRINTING================

% last route in the trip
print_single_trip([(Tid,_,_)|[]], Start, End) :-
    (
        % check if the route is one way or the other
        route(Tid, Start, End, Type, _, _)
    ;
        route(Tid, End, Start, Type, _, _)
    ),
    write(Start), write(' -('), write(Tid), write(','),
    write(Type), write(')-> '), write(End), nl.

% not the last route in the trip
print_single_trip([(Tid, From, _)|Tids], Start, End) :-
    (
        % check if the route is one way or the other
        route(Tid, From, Next, Type, _, _)
    ;
        route(Tid, Next, From, Type, _, _)
    ),
    write(Start), write(' -('), write(Tid), write(','), write(Type),
    write(')-> '),
    print_single_trip(Tids, Next, End).

print_all_trips([], _, _, _).

print_all_trips([(Tids, Length) |Trips], Start, End, Conditions) :-
    (
        check_length(Conditions, Length),
        print_single_trip(Tids, Start, End),
        write('Route length: '), write(Length), write('.'), nl, nl
    ),
    print_all_trips(Trips, Start, End, Conditions).

% ===============SEARCH ENGINE================
find_and_print_all_trips(Start, End, Conditions) :-
    search_all_trips(Start, End, Conditions, Trips),
    Trips \= [] ->
        % sorting removes duplicates
        sort(Trips, UniqueTrips),
        print_all_trips(UniqueTrips, Start, End, Conditions).

search_all_trips(Start, End, Conditions, Trips) :-
    search_all_trips(Start, End, Conditions, [], UniqueTrips),
    % sorting removes duplicates
    sort(UniqueTrips, Trips),
    !.

search_all_trips(Start, End, Conditions, Acc, Trips) :-
    generate_trip_meeting_conditions(Start, End, Tids,
        Length, Conditions),
    % \+ member checks if the pair (Tids, Length) already exists in Acc
    \+ member((Tids, Length), Acc),
    !,
    search_all_trips(Start, End, Conditions,
        [(Tids, Length) | Acc], Trips).

search_all_trips(_, _, _, Acc, Acc).

% ===============SEARCH ENGINE WHEN START OR END IS NIL================
find_list_of_all_starts(List) :-
    find_starts([], List).

find_starts(Acc, List) :-
    (route(_, Place, _, _, _, _); route(_, _, Place, _, oba, _)),
    \+ member(Place, Acc), !,
    find_starts([Place|Acc], List).

find_starts(List, List).

find_list_of_all_ends(List) :-
    find_ends([], List).

find_ends(Acc, List) :-
    (route(_, Place, _, _, oba, _); route(_, _, Place, _, _, _)),
    \+ member(Place, Acc), !,
    find_ends([Place|Acc], List).

find_ends(List, List).

find_and_print_trips_when_start_is_nil(End, Conditions) :-
    find_list_of_all_starts(StartList),
    process_starts(StartList, End, Conditions).

process_starts([Start|Rest], End, Conditions) :-
    (
        search_all_trips(Start, End, Conditions, Trips),
        sort(Trips, UniqueTrips),
        print_all_trips(UniqueTrips, Start, End, Conditions)
    ),
    process_starts(Rest, End, Conditions).

process_starts([], _, _).

find_and_print_trips_when_end_is_nil(Start, Conditions) :-
    find_list_of_all_ends(EndList),
    process_ends(EndList, Start, Conditions).

process_ends([], _, _).
process_ends([End|EndList], Start, Conditions) :-
    (
        search_all_trips(Start, End, Conditions, Trips),
        sort(Trips, UniqueTrips),
        print_all_trips(UniqueTrips, Start, End, Conditions)
    ),
    process_ends(EndList, Start, Conditions).

find_and_print_trips_when_start_and_end_are_nil(Conditions) :-
    find_list_of_all_starts(StartList),
    find_list_of_all_ends(EndList),
    process_starts_and_ends(StartList, EndList, Conditions).

process_starts_and_ends([], _, _).
process_starts_and_ends([Start|StartList], EndList, Conditions) :-
    process_ends(EndList, Start, Conditions),
    process_starts_and_ends(StartList, EndList, Conditions).

%==============Converting user input tuples to their list representation==============

convert_input_tuples_to_list((X,T), [X|L]) :-
    convert_input_tuples_to_list(T, L).

convert_input_tuples_to_list((X), [X]) :-
    X \= (_,_).

% ===============CHECKING INPUT DATA================
check_if_length_condition_is_correct(Conditions, NumLengths) :-
    NumLengths =:= 1 ->
    (
        member(length(Op, D), Conditions),
        ValidOps = [eq, lt, le, gt, ge],
        \+ member(Op, ValidOps) -> write('Error: invalid condition - '),
        write("length("), write(Op), write(","),
        write(D), write(")"), nl, fail ; true

    ); true.

check_length_condition_validity(Conditions) :-
    check_number_of_lengths_in_data(Conditions, NumLengths),
    member(NumLengths, [0,1]),
    check_if_length_condition_is_correct(Conditions, NumLengths).

check_number_of_conditions(Conditions, Difference) :-
    check_number_of_types_in_data(Conditions, NumTypes),
    check_number_of_lengths_in_data(Conditions, NumLengths),
    Total is NumTypes + NumLengths,
    length(Conditions, NumConditions),
    Difference is NumConditions - Total.

find_invalid_condition_and_print_error([Condition|Conditions]) :-
    \+ (
        Condition = length(_, _),
        Condition = type(_)
    ) -> write("Error: invalid condition provided: "),write(Condition),nl
    ;
    find_invalid_condition_and_print_error(Conditions).

read_conditions(Conditions) :-
    write('Enter conditions: '),
    read(CondTuples),
    convert_input_tuples_to_list(CondTuples, Conditions).

check_conditions_validity(Conditions, PossibleNewConditions) :-
    (
        (
            (Conditions == [nil] ; Conditions == [end]),
            PossibleNewConditions = Conditions,
            true
        )
        ;
        (
            Conditions \= [nil], Conditions \= [end],
            \+ check_length_condition_validity(Conditions),
            read_conditions(NewConditions),
            check_conditions_validity(NewConditions, PossibleNewConditions)
        )
        ;
        (
            Conditions \= [nil], Conditions \= [end],
            check_number_of_conditions(Conditions, NumInvalid),
            NumInvalid =:= 0,
            PossibleNewConditions = Conditions,
            true
        )
        ;
        (
            Conditions \= [nil], Conditions \= [end],
            check_number_of_conditions(Conditions, NumInvalid),
            NumInvalid > 0,
            find_invalid_condition_and_print_error(Conditions),
            read_conditions(NewConditions),
            check_conditions_validity(NewConditions, PossibleNewConditions)
        )
        ;
        PossibleNewConditions = Conditions
    ).

read_start(S) :-
    write('Enter starting point: '),
    read(S).

read_end(E) :-
    write('Enter ending point: '),
    read(E).

% ===============MAIN LOOP================
start :-
    main_loop.

main_loop :-
    read_start(Start),
    (Start == end -> write('Program ended. Happy hiking!'), nl, !;
    (
        read_end(End),
        (End == end -> write('Program ended. Happy hiking!'), nl, !;
            (
                read_conditions(CondList),
                check_conditions_validity(CondList, Conditions), nl,
                (Conditions == [end] ->
                    write('Program ended. Happy hiking!'), nl, !;
                ((
                    Start==nil,
                    End==nil,
                    find_and_print_trips_when_start_and_end_are_nil(Conditions)
                )
                ;
                (
                    Start == nil,
                    End \= nil,
                    find_and_print_trips_when_start_is_nil(End, Conditions)
                )
                ;
                (
                    End == nil,
                    Start \= nil,
                    find_and_print_trips_when_end_is_nil(Start, Conditions)
                );
                (
                    Start \= nil,
                    End \= nil,
                    find_and_print_all_trips(Start, End, Conditions)
                );
                true
                ),
                main_loop
                )
            )
        )
    )).
