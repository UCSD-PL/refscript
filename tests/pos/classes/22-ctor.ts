
class A<M extends ReadOnly> {

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
