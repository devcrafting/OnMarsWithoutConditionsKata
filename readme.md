[Software Craftsmanship Lyon Coding Dojo](http://www.meetup.com/fr-FR/Software-Craftsmanship-Lyon/events/231037693/) on Mars Rover kata with "no conditions" constraints.

Tried it with @fpellet in F# (given no condition = no pattern matching), it was a bit hard because we did not know well OO F# syntax, and disturbing because it was much more painful than with pattern matching, without being really prettier. But good experience to be constrained.

NB: at the end, we tried using computational expressions to declare sequence of actions the Mars Rover should do (instead of char array as in the original kata description), it is really cool :)! An example

    instructions {
        move
        pivotLeft
        move
        move
        pivotRight
    }  