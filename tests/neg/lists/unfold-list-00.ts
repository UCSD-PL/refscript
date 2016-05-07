
interface MyList<M extends ReadOnly,A> {
    d: A;
    /*@ n: MyList<M,A> + null */
    n: MyList<M,A>;
}

let inner = { d: 2, n: null };

/*@ readonly a :: MyList<Mutable, number> */
let a = { d: 1, n: inner };
