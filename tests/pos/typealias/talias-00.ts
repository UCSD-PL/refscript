/*@ alias NNat = {v: number | 0 <= v } */

/*@ z :: #NNat */
var z = 12;

/*@ foo :: (#NNat) => void */
function foo(a) { }

/*@ bar :: (#NNat) => void */
function bar(a) { }
