

/*@ option --extrainvs */


// Arrays
var trees = ["redwood", "bay", "cedar", "oak", "maple"];
assert(0 in trees);        // returns true
assert(3 in trees);        // returns true

assert(6 in trees);        // returns false


