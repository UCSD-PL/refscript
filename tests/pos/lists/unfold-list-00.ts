
interface MyList<M extends ReadOnly, A> {
    d: A;

    /*@ n : MyList<M,A> + null */
    n: MyList<M,A>;
}

let inner: MyList<Mutable, number> = { d: 2, n: null }

/*@ a :: MyList<Mutable, number> */
let a =  { d: 1, n: inner };

let b = a;
assert(a.d > 0);
