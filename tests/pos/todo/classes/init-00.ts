class PosPoint { 
    /*@ x: [#Mutable]{ number | v >= 0 } */
    x: number;

    /*@ y: [#Mutable]{ number | v >= 0 } */
    y: number;

    /*@ new (a: number, b: number) => PosPoint<M> */
    constructor(a: number, b: number) {
        this.x = a - a; 
        this.y = b - b;
    }
}
