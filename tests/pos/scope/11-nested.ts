
/*@ qualif AddTrhree(v:int, w: int): v = w + 3 */

/*@ foo :: () => { number | v = 4 } */
export function foo() {
    let u = 1;

    function addThree(u: number) {
        return u + 3;
    }

    return addThree(u);
}

let w = 1;

class ABC<M extends ReadOnly> {
    constructor() { }

    public addThree(u: number) {
        return u + 3;
    }
}

let a = new ABC();
assert(a.addThree(w) === 4);
