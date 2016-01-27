
/*@ qualif AddTrhree(v:int, w: int): v = w + 3 */

let w = 1;

class ABC<M extends ReadOnly> {
    constructor() { }

    public addThree(u: number) {
        return u + 3;
    }
}

let a: ABC<Immutable> = new ABC();

assert(a.addThree(w) === 4);
