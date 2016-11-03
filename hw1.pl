/* Program: hw1.pl
 * Authors: Brandon Rullamas and Christina Duran
 * On this homework, we worked together for 2 hours,
 * Brandon worked independently for 4 hours,
 * and Christina worked independently for 2 hours.
 * brullama and crduran
 * Flanagan
 */

/*Facts*/
father(al, bud).
father(al, kelly).
mother(peggy, kelly).
mother(peggy, bud).
mother(martha, peggy).

/*The mother of a child who is also a parent*/
grandma(X,Y) :- 
    (mother(Z,Y), mother(X,Z));
    (father(G,Y), mother(X,G)).

/*Children of children*/
descendants(X,Y) :-
    mother(X,Y);
    father(X,Y);
    (father(G,Y), descendants(X,G));
    (mother(G,Y), descendants(X,G)).

/*Children that share the same parents, X and Y are not the same child*/
siblings(X,Y) :-
    ((mother(G,X), mother(G,Y)); ((father(Z,X), father(Z,Y)))),
    (X \= Y).

/*Facts about NDFA "ab|aa*"*/
transition(q0,q1,a).
transition(q1,q2,b).
transition(q0,q3,a).
transition(q3,q3,a).
accepting(q2).
accepting(q3).

/*Given starting state and list of transitions, will the NDFA accept it*/
accepts(State, []) :- accepting(State).
accepts(State, [H|T]) :-
    (transition(State, X, H), accepts(X, T)).
