room(kitchen).
room(office).
room(cellar).
room(hall).
room('dining room').

location(broccoli, kitchen).
location(crackers, kitchen).
location(apple, kitchen).
location(desk, office).
location(computer, office).
location('washing machine', cellar).
location(flashlight, desk).
location(blankie, 'washing machine').

door(office, hall).
door(kitchen, office).
door(hall, kitchen).
door(hall, 'dining room').
door(kitchen, cellar).

% List things in a place
list_things(Place) :-
    location(X, Place),
    format("~w~n", [X]),
    fail.
list_things(_).

% Look in a place
look_in(Place, Container) :-
    format("The ~w contains:~n", [Container]),
    list_things(Container),
    location(Container, Place).

connect(X, Y) :- door(X, Y).
connect(X, Y) :- door(Y, X).
% List connections from a room
list_connections(Place) :-
    connect(Place, X),
    format("~w~n", [X]),
    fail.
list_connections(_).

% Can go to a room from curr
can_go(Place) :-
    here(X),
    connect(X, Place).


edible(crackers).
edible(apple).
% yucky
tastes_yucky(X) :- \+ edible(X).

% Initial
turned_off(flashlight).
here(kitchen).