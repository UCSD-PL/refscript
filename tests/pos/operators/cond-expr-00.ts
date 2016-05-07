
/*@ main :: (x: number) => { v:number | v != 0 } */
function main(x: number): number {
    return x ? x : (x + 1);
}
