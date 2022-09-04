<h1 align="center">Deterministic Finite Automata</h2>

<div align="center">
  <img src="https://user-images.githubusercontent.com/65187002/188329602-69cc62c8-f575-418e-a5ac-d350de35dd0d.png">
</div>

# 🔄 Description 🔄

[Deterministic Finite Automata](https://en.wikipedia.org/wiki/Deterministic_finite_automaton) (DFA) simulator written in [Prolog](https://en.wikipedia.org/wiki/Prolog).

<br>

# 📃 Definitions 📃

We define a deterministic finite automaton (DFA) as a tuple:

$$ \langle Q, \Sigma, \delta, q_0, F \rangle $$

where:
- $Q$ is a finite set of states
- $\Sigma$ is a finite alphabet
- $\delta: Q \times \Sigma \rightarrow Q$ is a transition function
- $q_0 \in Q$ is an initial state
- $F \subseteq Q$ is a set of accept states

We say that an automaton *accepts* a word $w = a_1a_2\dots a_n,\ n \geq 0$ if:

$$ \delta(q_0, a_1) = q_1, \delta(q_1, a_2) = q_2, \dots,\delta(q_{n-1}, a_n) = q_n $$

and state $q_n$ is an accept state.

A language accepted by an automaton $A$ is defined as a set of words accepted by that automaton:

$$ L(A) = \\{ w: \text{automaton} \ A \ \text{accepts the word} \ w \\} $$

<br>

# 🔀 Specification 🔀

A DFA is represented by terms of the form:

`dfa(+TransitionFunction, +InitialState, -AcceptStatesSet)`

where:
- `TransitionFunction` is a list of terms of form `fp(S1, C, S2)` meaning, that $\delta(S1, C) = S2$
- `InitialState` is an initial state of automaton
- `AcceptStatesSet` is a list of all unique accept states of an automaton

<br>

# ⏺ Predicates ⏺

## `correct(+Automaton, -Representation)`
True $\iff$ `Automaton` is a term representing a deterministic finite automaton and `Representation` is a non-complex term representing internal automaton structure.

<br>

## `accept(+Automaton, ?Word)`
True $\iff$ given `Automaton` accepts the word `Word`.
The parameter `Word` can be a non-complex term (a list of elements), as well as a complex term.

In case when `Word` is a complex term but a closed list, the predicate acts as a generator of words that complies with given pattern and therefore is accepted by the given `Automaton`.

In case when `Word` is a variable, the predicate acts as a generator of all the words belonging to the language accepted by the `Automaton`.

<br>

## `empty(+Automaton)`
True $\iff$ the language accepted by the `Automaton` is empty.

<br>

## `equal(+Automaton1, +Automaton2)`
True $\iff$ $L($`Automaton1`$) = L($`Automaton2`$)$ and the alphabets of both automata are equal.

<br>

## `subsetEq(+Automaton1, +Automaton2)`
True $\iff$ $L($`Automaton1`$) \subseteq L($`Automaton2`$)$ and the alphabets of both automata are equal.

<br>

# ✨ Examples ✨

`example(+AutomatonId, +Automaton)`


```prolog
example(a11, dfa([fp(1,a,1), fp(1,b,2), fp(2,a,2), fp(2,b,1)], 1, [2,1])).

example(a12, dfa([fp(x,a,y), fp(x,b,x), fp(y,a,x), fp(y,b,x)], x, [x,y])).

example(a2, dfa([fp(1,a,2), fp(2,b,1), fp(1,b,3), fp(2,a,3), fp(3,b,3), fp(3,a,3)], 1, [1])).

example(a3, dfa([fp(0,a,1), fp(1,a,0)], 0, [0])).

example(a4, dfa([fp(x,a,y), fp(y,a,z), fp(z,a,x)], x, [x])).

example(a5, dfa([fp(x,a,y), fp(y,a,z), fp(z,a,zz), fp(zz,a,x)], x, [x])).

example(a6, dfa([fp(1,a,1), fp(1,b,2), fp(2,a,2), fp(2,b,1)], 1, [])).

example(a7, dfa([fp(1,a,1), fp(1,b,2), fp(2,a,2), fp(2,b,1), fp(3,b,3), fp(3,a,3)], 1, [3])).
```

##

Automata `a11` and `a12` accept words of form $\\{ a, b \\}*$, so words over the alphabet $\Sigma = \\{ a, b \\}$.

Automaton `a2` accepts words of form $(ab)^n, \  n \geq 0$.

Automaton `a3` accepts words of form $(aa)^n, \  n \geq 0$, automaton `a4` accepts words of form $(aaa)^n, \  n \geq 0$ and automaton `a5` accepts words of form $(aaaa)^n, \  n \geq 0$.

The languages accepted by automata `a6` and `a7` are empty, since the set of accept states of automaton `a6` is empty, and the only accept state of automaton `a7` is not reachable from the initial state.

<br>

# ⏩ Example usage ⏩

```
sudo apt-get update && sudo apt-get install swi-prolog
swipl
consult('dfa.pl').
consult('examples.pl').
```

The following predicates should be true:

```prolog
example(a11, A), example(a12, B), equal(A, B).
example(a2, A), example(a11, B), subsetEq(A, B).
example(a5, A), example(a3, B), subsetEq(A, B).
example(a6, A), empty(A).
example(a7, A), empty(A).
example(a2, A), accept(A, []).
example(a2, A), accept(A, [a,b]).
example(a2, A), accept(A, [a,b,a,b]).
```

The following predicates should be false:

```prolog
example(a2, A), empty(A).
example(a3, A), example(a4, B), equal(A, B).
example(a4, A), example(a3, B), subsetEq(A, B).
example(a2, A), accept(A, [a]).
```

Predicate `example(a11, A), accept(A, [X,Y,Z]).` will generate 8 answers corresponding to all 3-letter words over the two-letter alphabet.

Predicate `example(a11, A), accept(A, Var).` will generate every word in the language accepted accepted by the automaton in a finite time.
