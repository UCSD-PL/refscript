/*@ alias Nat = {v: number | 0 <= v } */

/*@ z :: #Nat */
var z = 12;

/*@ foo :: (#Nat) => void */
function foo(a) { }

/*@ bar :: (#Nat) => void */
function bar(a) { }
