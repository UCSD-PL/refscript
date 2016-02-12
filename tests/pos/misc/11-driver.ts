/*@ qualif CondLock2(v: int, x: int, y: int): ((x = y) <=> (v != 0)) */
/*@ qualif Eq1(v: int): v != 0 */

// HINT: Recall the `invariant` for the corresponding test in tests/esc/pos
//       Find a way to represent that invariant as a qualifier (or a conjunction of qualifiers.)
//       You can use operators like <=>, =>, &&, || in the qualifiers.

/*@ create :: () => { number | v = 0 } */
export function create(): number {
    return 0;
}

function acquire(l: number): number {
    assert(l === 0);
    return 1;
}

function release(l: number): number {
    assert(l !== 0);
    return 0;
}

function driver(l: number, newCount: number, oldCount: number): number {
    if (newCount !== oldCount) {
        l = acquire(l);
        oldCount = newCount;
        if (0 < newCount) {
            l = release(l);
            newCount = newCount - 1;
        }
        else {
            newCount = newCount;
        }
        l = driver(l, newCount, oldCount);
    }
    return l;
}

export function main(): void {
    let newCount = pos();
    let oldCount = pos();
    let l = create();
    if (newCount < oldCount) {
        l = driver(l, newCount, oldCount);
        l = release(l);
    }
}
