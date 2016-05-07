
/*@ bob :: (number) => number + undefined */
function bob(x:number):any {
    if (x > 0) return x;
    return undefined;
}

/*@ bar :: (number) => number */
export function bar(x: number) : any {
    let z = bob(x);
    return z;
}
