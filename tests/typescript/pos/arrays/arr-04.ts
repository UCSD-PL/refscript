/*@ qualif OkLen(v:number, arr:a): v < (len arr) */

/*@ indirectIndex :: (a: [ number ], b: [ {number|((0 <= v) && (v < (len a)))} ], i: { number | ((0 <= v) && (v < (len b)))}) => number */
function indirectIndex(a : number[], b : number[], i : number) : number {
  return a[b[i]];

}

/*@ writeIndex :: (a:[number], i:{ number | (0 <= v && v < (len a)) }) => void */
function writeIndex(a : number[], i : number) : void {
  a[i] = 10;
  return;
}