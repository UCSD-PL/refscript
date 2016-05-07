
/*@ indirectIndex :: (x: IArray<number>, b: IArray<idx<x>>, i: idx<b>) => number */
function indirectIndex(x: number[], b: number[], i: number) : number {
    let bi = b[i];
    return x[bi];
}

/*@ writeIndex :: (x: IArray<number>, i: idx<x>, v : number) => void */
function writeIndex(x : number[], i : number, v: number) : void {
    // x[i] = v;        // Not supported
    return;
}
