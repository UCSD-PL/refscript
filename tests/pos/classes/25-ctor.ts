
class AA<M extends ReadOnly> {
    /*@ (Immutable) a: string */
    public a = "OLD";

    constructor() { }
}

class BB<M extends ReadOnly> extends AA<M> {

    /*@ (Immutable) a: { string | v = "NEW" } */
    public a: string;

    public b = 0;

    constructor() {
        super();
        this.a = "NEW";
    }
}

let n = new BB();
