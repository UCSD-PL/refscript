
class Foo<M extends ReadOnly> {
    /*@ bar : () => posint */
    public bar: () => number

    constructor() {
        /*@ _bar :: () => number */
        function _bar() {
            return -3;
        }

        this.bar = _bar;
    }
}
