
class A<M extends ReadOnly> {
    // Try changing Immutable to Mutable - it will fail because
    // the refinement on y can't refer to a mutable field

    /*@ (Immutable) x: number */
    public x: number;

    /*@ (Mutable) y: { number | v = this.x } */
    public y: number;

    constructor(a: number) {
        this.x = a;
        this.y = a;
    }
}

export function foo() {
    let r = new A(29);
    let p = r.x;
    r.y = r.x;
    let q = r.y;
    assert(p === q);
}
