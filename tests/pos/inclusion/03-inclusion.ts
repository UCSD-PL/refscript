
/*@ qualif Len(v: a): len v = 5 */

// Arrays
let trees: IArray<string> = ["redwood", "bay", "cedar", "oak", "maple"];
assert(0 in trees);        // returns true
assert(3 in trees);        // returns true
