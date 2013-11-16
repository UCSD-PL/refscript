


/*@ indirectIndex :: (a: [ number ], b: [ {number|((0 <= v) && (v <= (len a)))} ], i: { number | ((0 <= v) && (v < (len b)))}) => number */
function indirectIndex(a, b, i) {

  return a[ b[i] ];

}
