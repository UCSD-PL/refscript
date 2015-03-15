/*@ qualif Add(v: number, n: number, m: number): v = m + n */

// this should pass, but it doesn't - perhaps because we treat 
// "x[i++] += 3" as "x[i++] = x[i++] + 3" 
//   rather than as "x[i++] = x[i] + 3" ?

//adapted from navier-stokes
/*@ x :: {IArray<number> | len v = 40} */
declare var x;
var i = 0;
for (var j = 0; j < 21; j++) { // works if we reduce max j to 20
    x[i++] += 3;
}
