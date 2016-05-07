
interface NumList<M extends ReadOnly> {
    d: number;

    /*@ n : NumList<M> + null */
    n: NumList<M>;
}

export function foo() {

    let inner: NumList<Mutable> = { d: 2, n: null }

    /*@ a :: NumList<Mutable> */
    let a =  { d: 1, n: inner };

    let a_n = a.n;
    if (a_n)
        return a_n.d;
    return 0;
}

let aa = foo();
