:- module(story_dcg_html, [story//1]).

story(L) --> 
    { phrase(story_(L), StoryHtml) },
    [ ul([class(story)], StoryHtml) ].

story_([H|T]) --> step(H), story_(T).
story_([]) --> [].

atom(bathroom(X)) --> atom(X), [' bathroom'].
atom(Object) --> [Object].

step(Step) --> 
    { phrase(step_(Step), StepHtml) },
    [ li(StepHtml) ].

step_(pee) --> ['Use the restroom.'].
step_(wash_hands) --> ['Wash your hands!'].
step_(dress(For)) --> ['Dress for '], atom(For), ['.'].
step_(eat) --> ['Om nom nom.'].
step_(spill) --> ['Oh no! You spilled food on your shirt!'].
step_(grab(Object)) --> ['Pick up the '], atom(Object), ['.'].
step_(move(A,B)) --> ['Walk from the '], atom(A), [' to the '], atom(B), ['.'].
step_(drop(Object)) --> ['Put down the '], atom(Object), ['.'].
step_(needy_pet(Name, Animal)) --> ['~w the ~w demands love.'-[Name, Animal]].
step_(love_pet(Name, _Animal)) --> ['You pet ~w for a moment.'-[Name]].
