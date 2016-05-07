
/*@ qualif CmpZ (x: int, v: int): v = x + 1  */
/*@ qualif CmpZ (v: int): v != 0  */

/*@ main :: (x: number) => { v:number | v != 0 } */
function main(x: number): number {
    return x ? (x - 1) : (x + 1);
}
