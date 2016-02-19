

class Animal<M extends ReadOnly> {

    /* (Immutable) kind: { v: string | [((v = "") || (v = "horse") || (v = "snake") || (v = "tiger"));
                                          (v = "horse") => extends_class(this, "Horse");
                                          (v = "snake") => extends_class(this, "Snake")] } */

    /*@ (Immutable) kind: { v: string | v = "" || v = "horse" || v = "snake" || v = "tiger" } */
    public kind: string = "";

    /*@ new (): { Animal<M> | (this.kind = "horse" => extends_class this "Horse") &&
                              (this.kind = "snake" => extends_class this "Snake") }
    */
    constructor() {

    }
}

class Horse<M extends ReadOnly> extends Animal<M> {
    public gallop() { }
    constructor() {
        super();
        this.kind = "horse";
    }
}
class Snake<M extends ReadOnly> extends Animal<M> {
    public sneak() { }
    constructor() {
        super();
        this.kind = "snake";
    }
}

export function move(a: Animal<Immutable>): void {
    if (a.kind === "horse") {
        let h = <Horse<Immutable>>a;
        h.gallop();
    }
    else if (a.kind === "snake") {
        let s = <Snake<Immutable>>a;
        s.sneak();
    }
}
