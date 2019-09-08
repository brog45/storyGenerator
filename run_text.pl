:- use_module(planner).
:- use_module(story_data).
:- use_module(story_dcg_text).
:- use_module(story_generator).

go :-
    Name = 'Brian',
    Pet = 'Murray',
    Animal = 'cat',
    init(Name, Pet, Animal, State),
    generate_story(State, Story),
    writeln(Story),
    phrase(story(Story), StoryCodes),
    !,
    string_codes(StoryString, StoryCodes),
    writeln(StoryString).

% vim: et ts=4 sw=4 ai
