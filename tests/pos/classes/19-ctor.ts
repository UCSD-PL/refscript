

class A<M extends ReadOnly> {
    /*@ (Mutable) x: { number | v > 2 } */
    public x: number;

    /*@ (Mutable) y: string */
    public y: string;

    public c = "aaa";

    public foo() { return 1; }

    constructor() {
        if (random()) { this.y = "a"; } else { this.y = "b"; }
        if (random()) { this.x = 3; } else { this.x = 4; }
    }
}
