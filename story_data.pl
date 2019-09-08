% story data

:- module(story_data,[init/4, action/2, event/4]).

%! init(+Name:atom, +Pet:atom, +Animal:atom, -State:story_state)
init(Name, Pet, Animal, State) :-
    State = [ player_in(bedroom)
            , player(Name)
            , pet(Pet, Animal)
            , stomach(empty)
            , bladder(full)
            , hands(clean)
            , dressed_for(bed)
            , object_in(keys, bedroom)
            , object_in(comb, bathroom(master))
            % , goal(player_in(car))
            , goal(player_in(kitchen))
            , goal(stomach(full))
            , goal(bladder(empty))
            , goal(holding(keys))
            , goal(dressed_for(work))
            ].

% door(yard, car).
% door(den, yard).
door(den, kitchen).
door(hall, den).
% door(hall, bathroom(guest)).
door(bedroom, hall).
door(bedroom, bathroom(master)).
door(bathroom(master), closet).

connected_to(A,B) :- door(A,B).
connected_to(A,B) :- door(B,A).

% I think grouping each action with its corresponding 
% events makes this easier to read.
:- discontiguous action/2, event/4.

% pee
action(pee, action{
        prereqs: [player_in(bathroom(_)), bladder(full)],
        negprereqs: [holding(_)],
        removes: [hands(_), bladder(full)],
        adds: [hands(dirty), bladder(empty)]
    }).

% wash hands in the bathroom
action(wash_hands, action{
        prereqs: [player_in(bathroom(_)), hands(dirty)],
        negprereqs: [holding(_)],
        removes: [hands(dirty)],
        adds: [hands(clean)]
    }).

% dress for work
action(dress(work), action{
        prereqs: [player_in(closet)],
        negprereqs: [holding(_)],
        removes: [dressed_for(bed)],
        adds: [dressed_for(work)]
    }).

% wash hands in the kitchen
action(wash_hands, action{
        prereqs: [player_in(kitchen), hands(dirty)],
        negprereqs: [holding(_)],
        removes: [hands(dirty)],
        adds: [hands(clean)]
    }).

% eat
action(eat, action{
        prereqs: [player_in(kitchen), hands(clean), stomach(empty)],
        negprereqs: [holding(_)],
        removes: [stomach(empty)],
        adds: [stomach(full)]
    }).

event(eat, 0.5, spill, action{
        prereqs: [],
        negprereqs: [],
        removes: [dressed_for(work)],
        adds: []
    }).

% grab object
action(grab(Object), action{
        prereqs: [player_in(Location), object_in(Object, Location)],
        negprereqs: [holding(_)],
        removes: [object_in(Object, Location)],
        adds: [holding(Object)]
    }).

% move from room to room
action(move(CurrentLocation, Location), action{
        prereqs: [player_in(CurrentLocation)],
        negprereqs: [needy_pet],
        removes: [player_in(CurrentLocation)],
        adds: [player_in(Location)]
    }) :-
    connected_to(CurrentLocation, Location).

event(move(_, _), 0.1, needy_pet(Name, Animal), action{
        prereqs: [pet(Name, Animal)],
        negprereqs: [],
        removes: [],
        adds: [needy_pet]
    }).

% drop object
action(drop(Object), action{
        prereqs: [player_in(Location), holding(Object)],
        negprereqs: [],
        removes: [holding(Object)],
        adds: [object_in(Object, Location)]
    }).

% love pet
action(love_pet(Name,Animal), action{
        prereqs: [needy_pet, pet(Name, Animal)],
        negprereqs: [],
        removes: [needy_pet, hands(clean)],
        adds: [hands(dirty)]
    }).
