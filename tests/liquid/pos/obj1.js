/*@ foo :: ({v: number | (v > 0)}) => { a: {v: number | (v > 0)} }  */

function foo (x) {
  return { a: x };
}
