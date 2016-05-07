
export function foo(): void {
    let a = _pos();
    let b = random()

    if (a || b) {
        assert(true);
    }
    else {
        assert(false);
    }
}
