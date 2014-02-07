:-use_module(library(lists)).

test(L) :-
    tuple_calc([t,u],
    and(in(t,pc), and(in(u,pc), and(eq(attr(t,speed), attr(u,speed)),
    neg(eq(attr(t,model), attr(u,model)))))),
    L).

% works
test2(L) :-
    calc([[t,model]], in(t,simple), L).

test3(L) :-
    tuple_calc([t,u],
    and(in(t,pc), and(in(u,pc), neg(eq(attr(t,model), attr(u,model))))),
    L).

% works
testA(L) :-
    calc([[t,model]], and(in(t,pc), geq(attr(t,speed), 3.00)), L).

calc(Output, Formula, OutSet) :-
    maplist(head, Output, OutVars),
    tuple_calc(OutVars, Formula, BindSet),
    clean_outer(Output, BindSet, OutSet).

clean_outer(_, [], []).

clean_outer(Output, [H|T], Out) :-
    clean_inner(Output, H, RInner),
    clean_outer(Output, T, ROuter),
    append(RInner, ROuter, Out).

clean_inner([], _, []).

clean_inner([[T, []]|R], BindSet, OutSet) :-
    findall(X, member([T,X],BindSet),Temp),
    maplist(last, Temp, Tuples),
    clean(R, BindSet, Res),
    append(Res, Tuples, OutSet).

clean_inner([[T, A]|R], BindSet, OutSet) :-
    findall(X, member([T,X], BindSet), Temp),
    attr_clean(Temp, A, Filt),
    clean(R, BindSet, Res),
    append(Res, Filt, OutSet).

attr_clean([], _, []).

attr_clean([[Attr,X]|T], A, [Value|Res]) :-
    nth0(N, Attr, A),
    nth0(N, X, Value), 
    attr_clean(T, A, Res).

%% tuple_calc(+OutputSet, +Formula, ?SolutionSet)
%
% ex tuple_calc([[t,model],
%               and(in(pc,t), and(geq(attr(t,speed), 3)))
%               SolSet).


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

schema(simple, [model, speed]).
schema(pc, [model, speed, ram, hd, price]).

tuples(simple,
    [
       ['Hal', 100]
    ,  ['Intel', 200]
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
