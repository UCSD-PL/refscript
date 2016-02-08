

class A<M extends ReadOnly> {
    /*@ (Mutable) x: posint */
    public x: number;

    /*@ (Mutable) y: { string | (v = "a") || (v = "b") } */
    public y: string;

    public c = "aaa";

    public foo() { return 1; }

    constructor() {
        if (random()) { this.y = "a"; } else { this.y = "b"; }
        if (random()) { this.x = 2; } else { this.x = 3; }
    }
}
