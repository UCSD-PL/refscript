
function loop(x: number): number {
    if (x <= 4) {
        let r = loop(x + 1);
        return r;
    }
    return x;
}

export function main(): void {
    let x = loop(0);
    assert(x === 6);
}
