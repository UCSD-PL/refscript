
/*@ readonly a :: IArray<posint> */
let a = [1, 2, 3, 4];

function foo(): void {
    a[2] = 10;
}

/*@ bar :: (n: idx<a>) => {number | v > 0} */
function bar(n: number): number {
    return a[n];
}
