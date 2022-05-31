% ----------------------------------AUTHOR--------------------------------------


% University Of Warsaw
% Kamil Jankowski
% kj418271


% --------------------------------DESCRIPTION-----------------------------------


% The internal structure used for computations is based on Binary Search Trees.

% What is later called NodeBST is a BST with a triplet as a node.
% The triplet is (Q, D, F), where:
% Q - State
% D - FpBST containing all outgoing edges from Q in automaton
% F - 0/1, describing wheter Q is a final state

% Further explaining, FpBST is made up of nodes represented as p(A, Q), where:
% A - Letter from the alphabet
% Q - State

% Predicate r(W, E) stands for representation of some automaton, where W is
% a NodeBST and E is a normal BST containing alphabet letters.

% The state stored in the root of NodeBST is the initial state of automaton.


% -----------------------------------BST----------------------------------------


% fillUniqueBST(+BST, +Elements, -NewBST)
% --- true iff NewBST is BST with inserted distinct Elements list
% --- and none of the elements appeared in BST previously
fillUniqueBST(ND, [], ND).
fillUniqueBST(D, [H|T], D3) :-
    insertUniqueBST(D, H, D2),
    fillUniqueBST(D2, T, D3).

% insertUniqueBST(+BST, +Element, -NewBST)
% --- true iff Element does not appear in BST
% --- and NewBST is BST with inserted Element
insertUniqueBST(nil, X, node(nil, X, nil)).
insertUniqueBST(node(L, Y, R), X, node(NL, Y, R)) :-
    X@<Y,
    insertUniqueBST(L, X, NL).
insertUniqueBST(node(L, Y, R), X, node(L, Y, NR)) :-
    X@>Y,
    insertUniqueBST(R, X, NR).

% createBST(+List, -BST)
% --- true iff BST is BST with inserted Element
createBST(L, D) :-
    createBST(L, nil, D).
createBST([], D, D).
createBST([H | T], A, D) :-
    insertBST(A, H, AH),
    createBST(T, AH, D).

% insertBST(+BST, +Element, -NewBST)
% --- true iff NewBST is BST with inserted Element
insertBST(nil, X, node(nil, X, nil)).
insertBST(node(L, X, R), X, node(L, X, R)).
insertBST(node(L, Y, R), X, node(NL, Y, R)) :-
    X@<Y,
    insertBST(L, X, NL).
insertBST(node(L, Y, R), X, node(L, Y, NR)) :-
    X@>Y,
    insertBST(R, X, NR).

% createFlatBST(+List, -BST)
% --- true iff BST is made from List of elements in inorder
createFlatBST([], nil).
createFlatBST(L, node(D1, X, D2)) :-
    length(L, Len),
    Len \= 0,                   % length is not 0
    Y is (Len + 1) // 2 - 1,    % calculate length of left subtree
    length(P, Y),
    append(P, [X|S], L),
    createFlatBST(P, D1),
    createFlatBST(S, D2).

% lenBST(+BST, -Length)
% --- true iff Length is BST's size
lenBST(nil, 0).
lenBST(node(L, _, R), Len) :-
    lenBST(L, A),
    lenBST(R, B),
    Len is A+B+1.

% listBST(+BST, -List)
% --- true iff List represents BST's inorder traversal
listBST(D, L) :-
    listBST(D, [], L).
listBST(nil, L, L).
listBST(node(L, X, R), A, S) :-
    listBST(R, A, SR),
    listBST(L, [X|SR], S).

% listPreorderBST(+BST, -List)
% --- true iff List represents BST's preorder traversal
listPreorderBST(D, L) :-
    listPreorderBST(D, L, []).
listPreorderBST(nil, L, L).
listPreorderBST(node(L, X, R), [X|A], S) :-
    listPreorderBST(L, A, SL),
    listPreorderBST(R, SL, S).


% ---------------------------------correct--------------------------------------


% correct(+Automaton, -Representation)
% --- true iff Automaton is a correct deterministic automaton
% --- Representation is Automaton's internal structure
correct(dfa(FP, I, F), r(W3, E)) :-
    fillUniqueBST(nil, F, _),           % check if final states are distinct
    states(dfa(FP, I, F), S),           % BST tree of states
    alphabet(dfa(FP, I, F), E),         % BST tree of alphabet
    lenBST(E, Len),                     % the length of alphabet
    Len \= 0,                           % alphabet cannot be empty
    nodeBST(S, W),                      % main structure
    fillFP(W, FP, W2),                  % fill main structure with transitions
    setFinals(W2, F, W3),               % set final states
    fpSize(W3, Len).                    % assert all states have all transitions

% alphabet(+Automaton, -BST)
% --- true iff BST is Automaton's state BST Tree
states(dfa(FP, I, F), S) :- 
    createBST([I|F], D),
    states(FP, D, S).
states([], S, S).
states([fp(X, _, Z)|T], D, S) :-
    insertBST(D, X, D2),
    insertBST(D2, Z, D3),
    states(T, D3, S).

% alphabet(+Automaton, -BST)
% --- true iff BST is Automaton's alphabet BST Tree
alphabet(dfa(FP, _, _), S) :-
    alphabet(FP, nil, S).
alphabet([], S, S).
alphabet([fp(_, X, _)|T], D, S) :-
    insertBST(D, X, D2),
    alphabet(T, D2, S).

% fillFP(+NodeBST, +FpList, -NewNodeBST)
% --- true iff NewNodeBST is NodeBST with 
fillFP(D, [], D).
fillFP(D, [H|T], D3) :-
    insertFP(D, H, D2),
    fillFP(D2, T, D3).

% insertFP(+FpBST, +Fp, -NewFpBST)
% --- true iff NewFpBST is FpBST with transition stored in Fp
insertFP(node(L, (Q, D, F), R), fp(Q, Y, Q2), node(L, (Q, D2, F), R)) :- 
    insertUniqueBST(D, p(Y, Q2), D2).
insertFP(node(L, (S, D, F), R), fp(Q, Y, Q2), node(NL, (S, D, F), R)) :-
    Q@<S,
    insertFP(L, fp(Q, Y, Q2), NL).
insertFP(node(L, (S, D, F), R), fp(Q, Y, Q2), node(L, (S, D, F), NR)) :-
    Q@>S,
    insertFP(R, fp(Q, Y, Q2), NR).

% nodeBST(+StateBST, -NodeBST)
% --- true iff NodeBST is StateBST with nodes
% --- converted to triples (State, nil, 0)
nodeBST(nil, nil).
nodeBST(node(L, X, R), node(L2, (X, nil, 0), R2)) :-
    nodeBST(L, L2),
    nodeBST(R, R2).

% setFinal(+NodeBST, +FinalState, -NewNodeBST)
% --- true iff NewNodeBST is NodeBST with FinalState marked as final
setFinal(node(L, (Q, D, _), R), Q, node(L, (Q, D, 1), R)).
setFinal(node(L, (Q, D, F), R), X, node(L2, (Q, D, F), R)) :- 
    X@<Q,
    setFinal(L, X, L2).
setFinal(node(L, (Q, D, F), R), X, node(L, (Q, D, F), R2)) :- 
    X@>Q,
    setFinal(R, X, R2).

% setFinals(+NodeBST, +FinalStateList, -NewNodeBST)
% --- true iff NewNodeBST is NodeBST with FinalStateList states marked as final
setFinals(D, [], D).
setFinals(D, [H|T], D3) :-
    setFinal(D, H, D2),
    setFinals(D2, T, D3).

% fpSize(+NodeBST, +FpSize)
% --- true iff FpBST in every NodeBST's node is of size FpSize
fpSize(nil, _).
fpSize(node(L, (_, D, _), R), S) :-
    lenBST(D, S),
    fpSize(L, S),
    fpSize(R, S).


% --------------------------------complement------------------------------------


% complement(+Representation, -NewRepresentation)
% --- true iff NewRepresentation is the complement of automaton's Representation
% --- and their alphabets are equal
complement(r(W, E), r(W2, E)) :-
    swapFinal(W, W2).

% swapFinal(+NodeBST, -NewNodeBST)
% --- true iff NewNodeBST is NodeBST with swapped final states
swapFinal(nil, nil).
swapFinal(node(L, (Q, D, F), R), node(L2, (Q, D, F2), R2)) :-
    F2 is 1-F,          % final states are marked as 0/1
    swapFinal(L, L2),
    swapFinal(R, R2).


% -------------------------------intersection-----------------------------------


% intersection(+Representation1, +Representation2, -Intersection)
% --- true iff Intersection is the representation of automata
intersection(r(W1, E), r(W2, E2), r(W, E)) :-
    listBST(E, L),
    listBST(E2, L),             % check if alphabets are equal
    listPreorderBST(W1, Lst1),
    listPreorderBST(W2, Lst2),  % change BSTs to lists
    product(Lst1, Lst2, Lst3),  % cartesian product of automata
    createNodes(Lst3, WLst),    % list of nodes for NodeBST
    createBST(WLst, W).         % NodeBST of intersection
    
% product(+List1, +List2, -Product)
% --- true iff Product is the cartesian product of elements in List1 and List2
product(L, L2, L3) :-
    product(L, L2, L3, L).
product(_, [], [], _).
product([], [_|T2], List3, L) :-
    product(L, T2, List3, L).
product([H|T], [H2|T2], [[H,H2]|T3], L):-
    product(T, [H2|T2], T3, L).

% createNodes(+List, -NodeList)
% --- true iff NodeList is a list of NodeBST's node structure made from List
createNodes([], []).
createNodes([[(X, D, F),(Y, D2, F2)]|T], [([X,Y], D3, F3)|A]) :-
    F3 is F*F2,             % both states must be final
    merge(D, D2, D3),       % merge FpBSTs of both nodes into one
    createNodes(T, A).

% merge(+FpBST1, +FpBST2, -NewFpBST)
% --- true iff NewFpBST is a merged BST of
% --- transitions from FpBST1 and FpBST2
merge(D1, D2, D3) :-
    listBST(D1, L1),
    listBST(D2, L2),
    mergeFpLists(L1, L2, L3),
    createFlatBST(L3, D3).      % create balanced BST

% mergeFpLists(+List1, +List2, -NewList)
% --- true iff NewList is a list of transitions with
% --- merged states from List1 and List2
mergeFpLists(L1, L2, L3) :-
    mergeFpLists(L1, L2, [], L3).
mergeFpLists([], [], L, L).
mergeFpLists([p(X, Y)|T], [p(X, Z)|T2], A, L) :-
    mergeFpLists(T, T2, [p(X, [Y,Z])|A], L).


% ----------------------------------accept--------------------------------------


% accept(+Automaton, ?Word)
% --- true iff Automaton accepts Word
accept(A, S) :-
    var(S),                     % S is a variable
    correct(A, r(W, _)),
    infinite(W),                % A is inifinite
    acceptGen(W, 0, S).         % generate infinite language
accept(A, S) :-
    var(S),                     % S is a variable
    correct(A, r(W, _)),
    \+ infinite(W),             % A is finite
    lenBST(W, N),               % N states in A
    acceptLimit(W, 0, N, S).    % generate words up to length N
accept(A, S) :-
    \+ var(S),                  % S is not a variable
    acceptList(A, S).

% acceptGen(+NodeBST, +Length, -Word)
% --- true iff Word is a word of length >= Length and
% --- Automaton represented by NodeBST accepts it
acceptGen(W, K, S) :-
    acceptK(W, K, S).
acceptGen(W, K, S) :-
    K2 is K + 1,
    acceptGen(W, K2, S).

% acceptLimit(+NodeBST, +Length, +Limit, -Word)
% --- true iff Word is a word of length >= Length, length < Limit and
% --- Automaton represented by NodeBST accepts it
acceptLimit(W, K, _, S) :-
    acceptK(W, K, S).
acceptLimit(W, K, Lim, S) :-
    K2 is K + 1,
    K2 < Lim,
    acceptLimit(W, K2, Lim, S).

% acceptList(+Automaton, -Word)
% --- true iff Word is a list of letters from Automaton's alphabet and
% --- is a word accepted by Automaton
acceptList(A, S) :-
    correct(A, r(node(L, (I, D, F), R), _)),
    acceptList(node(L, (I, D, F), R), I, S).
acceptList(W, Q, [H|T]) :-
    findNodeBST(W, Q, (Q, D, _)),               % find state in NodeBST
    getBST(D, p(H, Q2)),                        % get all transitions
    acceptList(W, Q2, T).
acceptList(W, Q, []) :-
    findNodeBST(W, Q, (Q, _, 1)).

% acceptK(+NodeBST, +Length, -Word)
% --- true iff Word is a word of length Length and
% --- Automaton represented by NodeBST accepts it
acceptK(node(L, (I, D, F), R), K, S) :-
    acceptK(node(L, (I, D, F), R), I, K, S).
acceptK(W, Q, 0, []) :-
    findNodeBST(W, Q, (Q, _, 1)).
acceptK(W, Q, K, [H|T]) :-
    K>0,
    K2 is K-1,
    findNodeBST(W, Q, (Q, D, _)),               % find state in NodeBST
    getBST(D, p(H, Q2)),                        % get all transitions
    acceptK(W, Q2, K2, T).

% infinite(+NodeBST)
% --- true iff the language of automaton represented by NodeBST is infinite
%
% --- the language accepted by a DFA M with n states is infinite if and only if
% --- M accepts a string of length k, where n <= k < 2n
infinite(W) :-
    lenBST(W, N),
    N2 is N*2,
    infinite(W, N, N2).
infinite(W, K, _) :-
    acceptK(W, K, _), !.    % need one success to prove the infinity of language
infinite(W, K, N2) :-
    K2 is K + 1,
    K2 < N2,
    infinite(W, K2, N2).

% getBST(+BST, -Node)
% --- true iff Node is a node in BST
getBST(node(_, X, _), X).
getBST(node(L, _, _), X)  :-
    getBST(L, X).
getBST(node(_, _, R), X)  :-
    getBST(R, X).


% ----------------------------------empty---------------------------------------


% empty(+Automaton)
% --- true iff the language of Automaton is empty
empty(A) :-
    correct(A, r(W, _)),
    \+ nonEmpty(W).

% nonEmpty(+NodeBST)
% --- true iff the language of automaton stored in NodeBST is non-empty
% --- true iff any final state is reachable from initial state
nonEmpty(node(L, (I, D, F), R)) :-
    nonEmpty(node(L, (I, D, F), R), I, nil).
nonEmpty(W, X, V) :-                % V - Visited
    insertUniqueBST(V, X, _),       % check if X was not previously visited
    findNodeBST(W, X, (X, _, 1)).   % check if X is a final state
nonEmpty(W, X, V) :-
    insertUniqueBST(V, X, V2),      % mark X as visited
    findNodeBST(W, X, (X, D, 0)),   % check if X is not a final state
    traverseTree(D, V2, W).         % traverse X's FpBST

% traverseTree(+FpBST, +VisitedBST, +NodeBST)
% --- true iff any state from FpBST is nonEmpty
traverseTree(node(_, p(_, Q), _), V, W) :- nonEmpty(W, Q, V).
traverseTree(node(L, _, _), V, W) :- traverseTree(L, V, W).
traverseTree(node(_, _, R), V, W) :- traverseTree(R, V, W).

% findNodeBST(+NodeBST, +State, -Node)
% --- true iff Node is a node in NodeBST whose state matches State
findNodeBST(node(_, (Q, D, F), _), Q, (Q, D, F)).
findNodeBST(node(L, (Y, _, _), _), X, Z) :-
    X@<Y,
    findNodeBST(L, X, Z).
findNodeBST(node(_, (Y, _, _), R), X, Z) :-
    X@>Y,
    findNodeBST(R, X, Z).


% --------------------------------subsetEq--------------------------------------


% subsetEq(+Automaton1, +Automaton2)
% --- true iff the language of Automaton1 is a subset of
% --- Automaton2's language and their alphabets are equal
subsetEq(A1, A2) :-
    correct(A1, R1),
    correct(A2, R2),
    complement(R2, C2),             % calculate the complement of A2
    intersection(R1, C2, r(W, _)),  % the intersection of A1 and the
    \+ nonEmpty(W).                 % complement of A2 should be empty


% ----------------------------------equal---------------------------------------


% equal(+Automaton1, +Automaton2)
% --- true iff the languages of Automaton1 and Automaton2 are equal
% --- and their alphabets are equal
equal(A1, A2) :-
    subsetEq(A1, A2),
    subsetEq(A2, A1).


% ------------------------------------------------------------------------------