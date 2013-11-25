

/*@ indirectIndex :: (a: [ number ], b: [ {number|((0 <= v) && (v < (len a)))} ], i: { number | ((0 <= v) && (v < (len b)))}) => number */
function indirectIndex(a, b, i) {
  return a[ b[i] ];

}

/*@ writeIndex :: (a:[number], i:{ number | (0 <= v && v < (len a)) }) => void */
function writeIndex(a) {
  a[i] = 10;
  return;
}
