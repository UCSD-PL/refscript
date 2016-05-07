

/*@ qualif Len(v: a): len v = 3 */
/*@ qualif Len(v: a): len v = 4 */
/*@ qualif Len(v: a): len v = 5 */
/*@ qualif Len(v: a): len v = 6 */

// Arrays
let trees: IArray<string> = ["redwood", "bay", "cedar", "oak", "maple"];


assert("mylength" in trees); // returns true (length is an assert property)
