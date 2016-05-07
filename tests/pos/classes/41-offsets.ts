
//adapted from navier-stokes

class Foo<M extends ReadOnly> {
    /*@ (Immutable) x:  number */
    x;
    /*@ new (x:number): { Foo<M> | offset(v,"x") = x } */
    constructor(x) {
        this.x = x;
    }
}

/*@ foo :: ({Foo<Immutable> | offset(v,"x") = 5}) => {number | v = 0} */
function foo(z) {
    return z.x - 5;
}
