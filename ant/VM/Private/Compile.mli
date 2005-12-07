
open Types;
open Runtime;

value compile_declarations : Scope.scope -> UCStream.istream -> unit;
value compile_expression   : Scope.scope -> UCStream.istream -> term;

