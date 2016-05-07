/// <reference path="include/d3.d.ts" />

d3.ascending = d3_ascending;

/*@ d3_ascending :: (a: number, b: number) => { number | [ (a < b => v = -1);
                                                           (a = b => v =  0);
                                                           (a > b => v =  1)] } */
/*@ d3_ascending :: <T> (T,T) => {number | 0 < 1} */
function d3_ascending(a, b) {
  return a < b ? -1 : a > b ? 1 : a >= b ? 0 : NaN;
}
