# storyGenerator
> Prolog program to generate a story of getting out the door in the morning

## Prerequisites
This code was written for and is known to work with [SWI-Prolog](https://www.swi-prolog.org) version 7.7.15. It uses dict structures, which were introduced with version 7.

## Running the Code

The `run.pl` file provides a web user interface for the story generator, so you can interactively enter story parameters. Here's what running it from the command-line looks like:

```
$ swipl -s run.pl -g go
% Started server at http://localhost:8080/
?- 
```

To shut down the web interface, enter `halt.` at the Prolog top-level prompt.

```
$ swipl -s run.pl -g go
% Started server at http://localhost:8080/
?- halt.
$ 
```

The `run_text.pl` file was created for testing the story generator. It does not provide an interactive UI; it sets parameters, runs the story generator once, and prints the results. Running from the command-line looks like this:

```
$ swipl -s run_text.pl -g go -g halt
[move(bedroom,bathroom(master)),pee,wash_hands,move(bathroom(master),closet),dress(work),move(closet,bathroom(master)),needy_pet(Murray,cat),love_pet(M
urray,cat),wash_hands,move(bathroom(master),bedroom),grab(keys),move(bedroom,hall),needy_pet(Murray,cat),love_pet(Murray,cat),move(hall,den),move(den,k
itchen),drop(keys),wash_hands,eat,grab(keys)]
Walk from the bedroom to the master bathroom.
Use the restroom.
Wash your hands!
Walk from the master bathroom to the closet.
Dress for work.
Walk from the closet to the master bathroom.
Murray the cat demands love.
You pet Murray for a moment. 
Wash your hands!
Walk from the master bathroom to the bedroom.
Pick up the keys.
Walk from the bedroom to the hall.
Murray the cat demands love.
You pet Murray for a moment. 
Walk from the hall to the den.
Walk from the den to the kitchen.
Put down the keys.
Wash your hands!
Om nom nom.
Pick up the keys.
$ 
```
