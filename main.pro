% kerem haktan kurt

:- ['cmpefarm.pro'].
:- init_from_map.


% 1- agents_distance(+Agent1, +Agent2, -Distance)

manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    Distance is abs(X2 - X1) + abs(Y2 - Y1).

agents_distance(AgentId1, AgentId2, Distance) :-
    manhattan_distance((AgentId1.get(x), AgentId1.get(y)), (AgentId2.get(x), AgentId2.get(y)), Distance).


% 2- number_of_agents(+State, -NumberOfAgents)

list_size([],0).
list_size([_|T],N) :- list_size(T, N1), N is N1 + 1.

number_of_agents(State, NumberOfAgents) :-
    State = [Agents, _, _, _],
    dict_pairs(Agents, _, Pairs),
    list_size(Pairs,NumberOfAgents).


% 3- value_of_farm(+State, -Value)

get_value(Key, Value) :-
    (value(Key, Value) -> true ; Value =0). % Check if the Value appears in procedure.

agents_total_value(_, [], 0, 0).
agents_total_value(State, [_|T], N, Value) :-
    agents_total_value(State, T, N1, Value1), 
    get_agent(State, N1, Agent),
    get_value(Agent.get(subtype), TempValue),
    Value is Value1 + TempValue,
    N is N1 + 1.

objects_total_value(_, [], 0, 0).
objects_total_value(State, [_|T], N, Value) :-
    objects_total_value(State, T, N1, Value1),
    get_object(State, N1, Object),
    get_value(Object.get(subtype), TempValue),
    Value is Value1 + TempValue,
    N is N1 + 1.

value_of_farm(State, Value) :-
    State = [Agents, Objects, _, _],
    dict_pairs(Agents, _, Pairs1),  
    dict_pairs(Objects, _, Pairs2),  
    agents_total_value(State, Pairs1, _, Value1),
    objects_total_value(State, Pairs2, _, Value2),
    Value is Value1 + Value2.



% 4- find_food_coordinates(+State, +AgentId, -Coordinates)

% Tuple to list
tuple_to_list([], []).

tuple_to_list([(X, Y) | T], [[X, Y] | Result]) :-
    tuple_to_list(T, Result).

convert_tuples([], []).
convert_tuples(Tuples, Lists) :-
    tuple_to_list(Tuples, Lists).

% List to tuple
list_to_tuple([], []).

list_to_tuple([[X, Y] | T], [(X, Y) | Result]) :-
    list_to_tuple(T, Result).

convert_lists([], []).
convert_lists(Lists, Tuples) :-
    list_to_tuple(Lists, Tuples).
%%

food_checker(_, [], _, []).

food_checker(wolf, [_-Value|Tail], Foods, Coordinates) :-
    Value = agents{children:_, energy_point:_, subtype: ItemSubtype, type:_, x:X, y:Y},
    (member(ItemSubtype, Foods) ->
        food_checker(wolf, Tail, Foods, RestCoords),
        Coordinates = [(X, Y) | RestCoords]
    ;   food_checker(wolf, Tail, Foods, Coordinates)
    ).

food_checker(Subtype, [_-Value|Tail], Foods, Coordinates) :-
    (   Subtype = cow
    ;   Subtype = chicken
    ),
    Value = object{subtype:ItemSubtype,type: _, x:X, y:Y},
    (member(ItemSubtype, Foods) ->
        food_checker(Subtype, Tail, Foods, RestCoords),
        Coordinates = [(X, Y) | RestCoords]

    ;   food_checker(Subtype, Tail, Foods, Coordinates)
    ).

find_food_coordinates(State, AgentId, Coordinates) :-
    get_agent(State, AgentId, Agent),
    State = [Agents, Objects, _, _],
    findall(Food, can_eat(Agent.get(subtype), Food), Foods),
    dict_pairs(Agents, _, Pairs1),
    dict_pairs(Objects, _, Pairs2),
    (   Agent.get(subtype) = wolf,
        food_checker(Agent.get(subtype), Pairs1, Foods, TempCoordinates)
    ;   food_checker(Agent.get(subtype), Pairs2, Foods, TempCoordinates)
    ),
    convert_tuples(TempCoordinates, Coordinates).


% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)

% Insert an element into a sorted list
insert_sorted(X, [], [X]).
insert_sorted((A1, B1, C1), [(A2, B2, C2)|T], [(A1, B1, C1), (A2, B2, C2)|T]) :-
    A1 =< A2.
insert_sorted(X, [H|T], [H|T1]) :-
    insert_sorted(X, T, T1).

% Sort a list of tuples
sort_tuples([], []).
sort_tuples([H|T], Sorted) :-
    sort_tuples(T, SortedTail),
    insert_sorted(H, SortedTail, Sorted).

my_append([], L, L).
my_append([X|L1], L2, [X|L3]) :-
    my_append(L1, L2, L3).

% Define dynamic connections
connected(Targets, Node, NextNode) :-
    member(Node, Targets),
    member(NextNode, Targets),
    Node \= NextNode,
    manhattan_distance(Node, NextNode, 1).

% Find the closest target using Manhattan distance
target_sorter(Start, Targets, DistanceSortedList) :-
    findall((Distance, Target),
            (member(Target, Targets),
             manhattan_distance(Start, Target, Distance)),
             Distances),
    sort_tuples(Distances, DistanceSortedList).

% Get Coordinates in desired format
group_coordinates([], []).
group_coordinates([_-agents{children:_, energy_point:_, subtype:_, type:_, x:X, y:Y} | Rest], [(X,Y) | Pairs]) :-
    group_coordinates(Rest, Pairs).

find_nearest_agent(State, AgentId, Coordinates, NearestAgent) :-
    get_agent(State, AgentId, Agent),
    Start = (Agent.x, Agent.y),
    State = [Agents, _, _, _],
    dict_pairs(Agents, _, AgentPairs),
    group_coordinates(AgentPairs, Targets),
    target_sorter(Start, Targets, AllCoordinates),
    AllCoordinates = [_|Tail],
    Tail = [(_,TupleCoordinates)|_],
    TupleCoordinates = (X,Y),
    Coordinates = [X,Y],
    get_agent_from_position(X,Y,Agents, NearestAgent).


% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)

find_nearest_food(State, AgentId, Coordinates,FoodType, Distance):-
    State = [Agents, Objects, _, _],
    get_agent(State, AgentId, Agent),
    Start = (Agent.x, Agent.y),
    find_food_coordinates(State, AgentId, ListCoordinates), % Find all food coordinates
    list_to_tuple(ListCoordinates, TupleCoordinates),
    target_sorter(Start, TupleCoordinates, SortedCoordinates), % Sort
    SortedCoordinates = [(Distance, X, Y)|_],
    Coordinates = [X,Y],
    (   Agent.get(subtype) = wolf,
        get_agent_from_position(X,Y,Agents,FoodAgent),
        FoodAgent = agents{children:_, energy_point:_, subtype:FoodType, type:_, x:_, y:_}

    ;   get_object_from_position(X,Y,Objects,FoodObject),
        FoodObject = object{subtype:FoodType, type:_, x:_, y:_}
    ).
