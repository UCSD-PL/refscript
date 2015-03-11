//adapted from navier-stokes
class Foo {
    /*@ a : [Immutable] {number | v = 5} */
    a = 5;
    constructor() {}
    /*@ bar : ({number | v = this.a}) : {number | v = 0} */
    bar(x) { return this.a - x }
}
var z = new Foo();
z.bar(4);
