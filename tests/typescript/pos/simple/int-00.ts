/* foo :: (x: {v:  boolean | ttag(v) = "number"} ) =>
{ v: number + boolean + string | ttag(v) = "string" } */

/*@ foo :: ( xxxx: top ) => top */
function foo(xxxx) {
    return xxxx;
}
