

class AA<M extends ReadOnly> {
    /*@ (Immutable) a: { string | v = "OLD" } */
    public a = "OLD";

    constructor() { }
}

class BB<M extends ReadOnly> extends AA<M> {

    public b = 0;

    constructor() {
        super();
        this.a = "NEW";
    }
}

let n = new BB();
