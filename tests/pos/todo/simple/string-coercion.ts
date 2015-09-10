
//adapted from transducers

/*@ bar :: (string) => void */
declare function bar(x);
/*@ foo :: ([Immutable]{ }) => void */
declare function foo(x);

/*@ reduce :: /\ (string) => {void | true}
              /\ ([Immutable]{ }) => {void | true} */
function reduce(coll) {
    if (typeof coll === "string") bar(coll)
    else foo(coll)
}
