/*@ alias nat = { number | 0 <= v } */

/*@ z :: #nat */
var z = 12;

/*@ foo :: (#nat) => void */
function foo(a) { }

/*@ bar :: (#nat) => void */
function bar(a) { }
