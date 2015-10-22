
/*@ indirectIndex :: (x: IArray<number>,
                      b: IArray<{ number | [ 0 <= v; v < len x ] }>, 
                      i:        { number | [ 0 <= v; v < len b ] }
                     ) 
                  => number 
 */
function indirectIndex(x: number[], b: number[], i: number) : number {
  let bi = b[i];
  return x[bi];
}

/*@ writeIndex :: (x: IArray<number>, 
                   i: { number | [ 0 <= v; v < len x ] }, 
                   v: number
                  ) 
               => void 
 */
function writeIndex(x : number[], i : number, v: number) : void {
  x[i] = v;
  return;
}
