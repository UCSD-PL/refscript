
//adapted from transducers

/*@ foo :: ([Immutable]{ }) => boolean */
declare function foo(x);

/*@ reduce :: (string) => {void | true} */
function reduce(coll) {
    if (false) foo(coll)
}
