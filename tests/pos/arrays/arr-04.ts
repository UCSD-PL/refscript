
/*@ indirectIndex :: (a: IArray<number>, b: IArray<{number|((0 <= v) && (v < (len a)))}>, i: { number | ((0 <= v) && (v < (len b)))}) => number */
function indirectIndex(a : number[], b : number[], i : number) : number {
  return a[b[i]];

}

/*@ writeIndex :: (a:IArray<number>, i:{ number | (0 <= v && v < (len a)) }, v: number) => void */
function writeIndex(a : number[], i : number, v: number) : void {
  a[i] = v;
  return;
}
