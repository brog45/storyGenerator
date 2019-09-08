:- module(story_dcg, [story//1]).

story([H|T]) --> step(H), "\n", story(T).
story([]) --> [].

atom(bathroom(X)) --> atom(X), " bathroom".
atom(Object) --> {atom_codes(Object, Codes)}, Codes.

step(pee) --> "Use the restroom.".
step(wash_hands) --> "Wash your hands!".
step(dress(For)) --> "Dress for ", atom(For), ".".
step(eat) --> "Om nom nom.".
step(spill) --> "Oh no! You spilled food on your shirt!".
step(grab(Object)) --> "Pick up the ", atom(Object), ".".
step(move(A,B)) --> "Walk from the ", atom(A), " to the ", atom(B), ".".
step(drop(Object)) --> "Put down the ", atom(Object), ".".
step(needy_pet(Name, Animal)) --> atom(Name), " the ", atom(Animal), " demands love.".
step(love_pet(Name, _Animal)) --> "You pet ", atom(Name), " for a moment. ".
