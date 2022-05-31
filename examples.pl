% dfa(+TransitionFunction, +Initial, -FinalList)
% --- DFA has initial state Initial, final states FinalList and
% --- transition function TransitionFunction

% fp(+State, +Letter, +State2)
% --- exists transition from State to State2 over Letter

% example(+Identificator, +Automaton)
% --- Automaton has id Identificator

% good automata
example(a11, dfa([fp(1,a,1), fp(1,b,2), fp(2,a,2), fp(2,b,1)], 1, [2,1])).
example(a12, dfa([fp(x,a,y), fp(x,b,x), fp(y,a,x), fp(y,b,x)], x, [x,y])).
example(a2, dfa([fp(1,a,2), fp(2,b,1), fp(1,b,3), fp(2,a,3), fp(3,b,3), fp(3,a,3)], 1, [1])).
example(a3, dfa([fp(0,a,1), fp(1,a,0)], 0, [0])).
example(a4, dfa([fp(x,a,y), fp(y,a,z), fp(z,a,x)], x, [x])).
example(a5, dfa([fp(x,a,y), fp(y,a,z), fp(z,a,zz), fp(zz,a,x)], x, [x])).
example(a6,  dfa([fp(1,a,1), fp(1,b,2), fp(2,a,2), fp(2,b,1)], 1, [])).
example(a7,  dfa([fp(1,a,1), fp(1,b,2), fp(2,a,2), fp(2,b,1), fp(3,b,3), fp(3,a,3)], 1, [3])).

% bad automata
example(b1, dfa([fp(1,a,1), fp(1,a,1)], 1, [])).
example(b2, dfa([fp(1,a,1), fp(1,a,2)], 1, [])).
example(b3, dfa([fp(1,a,2)], 1, [])).
example(b4, dfa([fp(1,a,1)], 2, [])).
example(b5, dfa([fp(1,a,1)], 1, [1,2])).
example(b6, dfa([], [], [])).