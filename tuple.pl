:-use_module(library(lists)).

% works
test2(L) :-
    calc([[t,model]], 
    and(in(t,simple), exist(u, and(in(u,simple), gt(attr(u,speed), attr(t,speed))))), 
    L).

% works
test3(L) :-
    tuple_calc([t,u],
    and(in(t,pc), and(in(u,pc), neg(eq(attr(t,model), attr(u,model))))),
    L).

% works
testA(L) :-
    calc([[t,model],[t,speed]], and(in(t,pc), geq(attr(t,speed), 3.00)), L).

% works
testF(L) :-
    calc([[t,hd]], and(in(t,pc), exist(u, and(in(u,pc),
    and(neg(eq(attr(t,model), attr(u,model))), eq(attr(t,hd), attr(u,hd)))))),
    L).

% works
testI(L) :-
    calc([[t,maker]], 
    and(in(t,product), 
    exist(x, and(in(x,pc), 
    and(eq(attr(x,model), attr(t,model)),
    neg(exist(y, and(in(y,pc),
    gt(attr(y,speed), attr(x,speed))))))))),
    L).

calc(Output, Formula, OutSet) :-
    maplist(head, Output, OutVars),
    tuple_calc(OutVars, Formula, BindSet),
    clean_outer(Output, BindSet, DupSet),
    % set semantics
    remove_dups(DupSet, OutSet).

%% clean_outer(+OutPutVars, +BindingSet, ?BagSolution)
%
%  extracts the relevant attributes from the tuple
%  and produces a bag semantics result
%  Cleans every solution subset of tuples t,u ...
%  in order
%  

clean_outer(_, [], []).

clean_outer(Output, [H|T], [RInner|ROuter]) :-
    clean_inner(Output, H, RInner),
    clean_outer(Output, T, ROuter).

clean_inner([], _, []).

clean_inner([[T, A]|R], BindSet, OutSet) :-
    findall(X, member([T,X], BindSet), Temp),
    attr_clean(Temp, A, Filt),
    clean_inner(R, BindSet, Res),
    append(Res, Filt, OutSet).

attr_clean([], _, []).

attr_clean([[Attr,X]|T], A, [Value|Res]) :-
    nth0(N, Attr, A),
    nth0(N, X, Value), 
    attr_clean(T, A, Res).

%% tuple_calc(+OutputSet, +Formula, ?SolutionSet)
%
%  calculates the solution set of tuples from the 
%  formula and output variable set
%

tuple_calc(OutVars, Formula, BindSet) :-
    findall(Re, bind(OutVars,[],Formula,Re), BindSet).

%%  bind
%   +V - Bound variables,
%   +B - Formula variable to Prolog variable bindings
%   +Formula 
%   ?Result
%

bind(Bound, Bindings, and(F1, F2), Res) :-
    bind(Bound, Bindings, F1, NBind),
    bind(Bound, NBind, F2, Res).

bind(Bound, Bindings, or(F1, F2), Res) :-
    bind(Bound, Bindings, F1, Res);
    bind(Bound, Bindings, F2, Res).

bind(Bound, Bindings, exist(U,F), Bindings) :-
    bind([U|Bound], Bindings, F, _). 

bind(Bound, Bindings, in(T, S), [[T,[A,X]]|Bindings]) :-
    member(T,Bound),
    schema(S, A),
    tuples(S, Tuples),
    member(X, Tuples).

bind(Bound, Bindings, neg(F), Bindings) :-
    \+ bind(Bound, Bindings, F, _).

bind(_, Bindings, eq(X1, X2), Bindings) :-
    value(X1, Bindings, R1),
    value(X2, Bindings, R2),
    R1 == R2.

bind(_, Bindings, geq(X1, X2), Bindings) :-
    value(X1, Bindings, R1),
    value(X2, Bindings, R2),
    R1 >= R2.

bind(_, Bindings, leq(X1, X2), Bindings) :-
    value(X1, Bindings, R1),
    value(X2, Bindings, R2),
    R1 =< R2.

bind(_, Bindings, lt(X1, X2), Bindings) :-
    value(X1, Bindings, R1),
    value(X2, Bindings, R2),
    R1 < R2.

bind(_, Bindings, gt(X1, X2), Bindings) :-
    value(X1, Bindings, R1),
    value(X2, Bindings, R2),
    R1 > R2.

%% value(+UnknownValue, +Bindings, ?Res)
%
% Convinience predicate to determine unknown value atoms
%

value(attr(T,A), Bindings, Res) :- attr(Bindings, T, A, Res).
value(X, _, X) :- number(X).

%% attr(+Bindings, +LogicVar, +Attribute, ?Value)
%
% Gets the attribute of logic var from existing bindings
%

attr(Bindings, T, Attribute, Value) :-
    member([T,[A,X]], Bindings),
    nth0(N, A, Attribute),
    nth0(N, X, Value). 

schema(simple, [model, speed, hd]).
schema(pc, [model, speed, ram, hd, price]).
schema(product, [maker, model, type]).

tuples(simple,
    [
       ['Hal', 100, 20]
    ,  ['Intel', 200, 20]
    ,  ['Haswell', 200, 30]
    ]).

tuples(pc, 
    [
        [1001, 2.66, 1024, 250, 2114]
    ,   [1002, 2.10, 512, 250, 995]
    ,   [1003, 1.42, 512, 80, 478]
    ,   [1004, 2.80, 1024, 250, 649]
    ,   [1005, 3.20, 512, 250, 630]
    ,   [1006, 3.20, 1024, 320, 1049]
    ,   [1007, 2.20, 1024, 200, 510]
    ,   [1008, 2.20, 2048, 300, 770]
    ,   [1009, 2.00, 1024, 250, 650]
    ,   [1010, 2.80, 2048, 160, 959]
    ,   [1011, 1.86, 2048, 300, 770]
    ,   [1012, 2.80, 1024, 160, 649]
    ,   [1013, 3.06, 512, 80, 529]
    ]).

tuples(product,
    [
        ['A', 1001, 'pc']
    ,   ['A', 1002, 'pc']
    ,   ['A', 1003, 'pc']
    ,   ['B', 1004, 'pc']
    ,   ['B', 1005, 'pc']
    ,   ['B', 1006, 'pc']
    ,   ['C', 1007, 'pc']
    ,   ['D', 1008, 'pc']
    ,   ['D', 1009, 'pc']
    ,   ['D', 1010, 'pc']
    ,   ['E', 1011, 'pc']
    ,   ['E', 1012, 'pc']
    ,   ['E', 1013, 'pc']
    ]).
