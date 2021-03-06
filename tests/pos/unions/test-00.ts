/*@ fails :: (number) => number + undefined */
export function fails(x: number): any {
    return x ? 10 : undefined;
}

/*@ ok1 :: (number) => number + undefined */
export function ok1(x: number): any {
    if (x) { return 10; } else { return undefined; }
}

/*@ ok2 :: (number) => number + undefined */
export function ok2(x: number): number {
    /*@ z :: number + undefined */
    let z;
    if (x) { z = 10; } else { z = undefined; }
    return z;
}
