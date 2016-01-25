/*@ readonly */ let a: number[] = [1, 2, 3, 4];

/*@ foo :: () => void */
function foo(): void {
    a[2] = 10;
}

/*@ bar :: ({ number | 0 <= v && v <= 4 }) => posint */
export function bar(n: number): number {
    let z: number = a[n];
    return z;
}
