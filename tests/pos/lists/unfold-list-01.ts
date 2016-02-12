
interface NumList<M extends ReadOnly> {
    d: number;

    /*@ n : NumList<M> + null */
    n: NumList<M>;
}

export function foo() {

    /*@ a :: NumList<Mutable> */
    let a =  { d: 1, n: { d: 2, n: null } };

    let a_n = a.n;
    if (a_n)
        return a_n.d;
    return 0;
}

let aa = foo();
