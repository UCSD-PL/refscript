
export function foo(): void {
    let a = pos();
    let b = random()

    if (a || b) {
        assert(true);
    }
    else {
        assert(false);
    }
}
