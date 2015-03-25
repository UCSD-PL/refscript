/// <reference path="../../../d3.d.ts" />

d3.ascending = d3_ascending;

/*@ d3_ascending :: /\ (a: number, b: number) => { number | [ (a < b => v = -1);
                                                              (a = b => v =  0);
                                                              (a > b => v =  1)] }
                    /\ forall T . (T,T) => {number | true}
 */
function d3_ascending(a, b) {
  return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
}
