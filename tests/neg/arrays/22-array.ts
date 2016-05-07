
/*@ readonly */ let aaa: IArray<number> = [1, 2, 3, 4];

/*@ foo :: () => void */
function foo(): void {
    aaa[2] = 0;
}

/*@ bar :: (n: idx<aaa>) => posint */
export function bar(n: number): number {
    let z = aaa[n];
    return z;
}
