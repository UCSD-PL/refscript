class PosPoint { 
    /*@ x: [#Mutable]{ number | v >= 0 } */
    x: number;

    /*@ y: [#Mutable]{ number | v >= 0 } */
    y: number;

    /*@ new (a: number, b: number) => PosPoint<M> */
    constructor(a: number, b: number) {
        if (a > 0 && b > 0) { this.x = a; this.y = b; }
        else                { this.x = 0; this.y = 0; }
    }
}
