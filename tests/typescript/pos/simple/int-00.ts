/*@ foo :: (x: {v:  boolean | ttag(v) = "number"} ) =>
{ v: number + boolean + string | ttag(v) = "string" } */

/*@ foo :: ( x: top ) => top */
function foo(x) {
    return x;
}
