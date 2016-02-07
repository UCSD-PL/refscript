
// Here we are not initializing `a` in contructor.

class Greeter<M extends ReadOnly> {
    /*@ a : string */
    a;

    /*@ b : number */
    private b;

    /*@ (Mutable) c : string */
    private c = "1";

    /*@ d : number */
    public  d ;

    /*@ e : { } */
    private static e = { };

    /*@ new (message: { string | v = "a" } ): Greeter<M> */
    constructor(message) { }

    /*@ greet1(): void */
    private greet1() {
        this.c = "a";
    }

    /*@ greet2(): void */
    greet2() { }

    /*@ greet3(): void */
    public greet3() { }
}
