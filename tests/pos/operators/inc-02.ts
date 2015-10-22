
/*@ inc :: (x: number) => { number | v = x + 1 } */
function inc(x: number): number {
    return x + 1;
}

export function main(): void {
    let a: number = pos();
    let b: number = inc(a);
    assert(b === (a + 1));
    assert(b > 0);
}
