/// <reference path="include/d3.d.ts" />

d3.descending = function(a, b)
/*@ <anonymous> /\ (a: number, b: number) => { number | [ (a > b => v = -1);
                                                          (a = b => v =  0);
                                                          (a < b => v =  1)] }
                /\ <T> (T,T) => {number | 0 < 1}
 */

{
  return b < a ? -1 : b > a ? 1 : b >= a ? 0 : NaN;
};
