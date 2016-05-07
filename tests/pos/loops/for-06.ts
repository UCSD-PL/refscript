
export function foo(): void {
    for (let x = 1; x < 3; x++) { }
}

export function bar(): void {
    let x;
    for (x = 1; x < 3; x++) { }
    assert(x >= 3);
}
