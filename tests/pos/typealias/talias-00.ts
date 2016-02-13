/*@ type Nat = {v: number | 0 <= v } */
type Nat = number;

/*@ z :: Nat */
let z = 12;

/*@ foo :: (Nat) => void */
function foo(a) { }

/*@ bar :: (Nat) => void */
function bar(a) { }
