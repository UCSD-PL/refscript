
class Foo<M extends ReadOnly> {

    public bar: () => number;

    constructor() {
        this.bar = <() => number> function() {
            return 3;
        }
    }
}
