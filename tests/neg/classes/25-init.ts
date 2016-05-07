class PosPoint<M extends ReadOnly> {
    /*@ (Mutable) x: { number | v >= 0 } */
    x: number;

    /*@ (Mutable) y: { number | v >= 0 } */
    y: number;

    constructor(a: number, b: number) {
        if (a > 0 && b > 0) {
            this.x = a;
            this.y = b;
        }
        else {
            this.x = 0;
            this.y = -1;
        }
    }
}

let pp = new PosPoint(0, 1);
