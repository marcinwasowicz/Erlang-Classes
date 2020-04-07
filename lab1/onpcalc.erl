-module(onpcalc).
-export([onp/2, calculator/1]).

onp([S], []) -> S;
onp([X | S], ["sqrt" | Tail]) -> onp([math:sqrt(X)] ++ S, Tail);
onp([X | S], ["sin" | Tail]) -> onp([math:sin(X)] ++ S, Tail);
onp([X | S], ["cos" | Tail]) -> onp([math:cos(X)] ++ S, Tail);
onp([X | S], ["tan" | Tail]) -> onp([math:tan(X)] ++ S, Tail);
onp([X | S], ["ctan" | Tail]) -> onp([1/math:tan(X)] ++ S, Tail);
onp([X | S], ["&" | Tail]) -> onp([math:sqrt(math:sin(X))] ++ S, Tail);
onp([X, Y | S], ["pow" | Tail]) -> onp([math:pow(X, Y)] ++ S, Tail);
onp([X, Y | S], ["+" | Tail]) -> onp([X+Y] ++ S, Tail);
onp([X, Y | S], ["-" | Tail]) -> onp([X-Y] ++ S, Tail);
onp([X, Y | S], ["/" | Tail]) -> onp([X/Y] ++ S, Tail);
onp([X, Y | S], ["*" | Tail]) -> onp([X*Y] ++ S, Tail);
onp([X, Y | S], ["@" | Tail]) -> onp([math:pow(X+Y, 2)] ++ S, Tail);
onp(S, [Head|Tail]) -> onp([erlang:list_to_float(Head)] ++ S, Tail).

calculator(Expr) -> onp([], string:tokens(Expr, " ")).
