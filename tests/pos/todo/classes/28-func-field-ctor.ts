
class Foo<M extends ReadOnly> {

    public bar: () => number;

    constructor() {
        this.bar = function() {
            return -3;
        }
    }
}
