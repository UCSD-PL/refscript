

/*@ qualif Len(v: a): len v = 3 */
/*@ qualif Len(v: a): len v = 4 */
/*@ qualif Len(v: a): len v = 5 */
/*@ qualif Len(v: a): len v = 6 */



// Arrays

let trees: IArray<string> = ["redwood", "bay", "cedar", "oak", "maple"];
assert(0 in trees);        // returns true
assert(3 in trees);        // returns true

assert(6 in trees);        // returns false
