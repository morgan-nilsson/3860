:- dynamic here/1.

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
door(kitchen, 'dining room').

% List things in a place
list_things(Place) :-
    location(X, Place),
    format("\t~w~n", [X]),
    fail.
list_things(_).

% Look in a place
look_in(Place, Container) :-
    format("The ~w contains:~n", [Container]),
    list_things(Container),
    location(Container, Place).

% connections are bidirectional
connect(X, Y) :- door(X, Y).
connect(X, Y) :- door(Y, X).

% List connections from a room
list_connections(Place) :-
    connect(Place, X),
    format("\t~w~n", [X]),
    fail.
list_connections(_).

% Can go to a room from curr
can_go(Place) :-
    here(X),
    connect(X, Place).
can_go(Place) :-
    format("You can't get to ~w from here.~n", [Place]),
    fail.

move(Place) :-
    can_go(Place),
    retract(here(_)),
    assert(here(Place)).

edible(crackers).
edible(apple).
% yucky
tastes_yucky(X) :- \+ edible(X).

look :-
    here(Place),
    format("You are in the ~w.~n", [Place]),
    format("You can see:~n"),
    list_things(Place),
    format("You can go to:~n"),
    list_connections(Place).

goto(Place) :-
    can_go(Place),
    move(Place),
    look.

% Initial
turned_off(flashlight).
here(kitchen).