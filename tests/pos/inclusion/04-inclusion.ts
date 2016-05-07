/*@ option --extrainvs */

// Arrays
let trees: IArray<string> = ["redwood", "bay", "cedar", "oak", "maple"];

assert("length" in trees); // returns true (length is an assert property)
