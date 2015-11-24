/*@ alias eq[s] = {number | v = s} */
class Foo {
    /*@ x : [Immutable] number */
    private x = 3;
    /*@ y : [Immutable] eq[this.x] */
    private y = 3;
    constructor() { }
}
