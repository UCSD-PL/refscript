
/*@ foo :: () => {void | 0 < 1}            */
/*@ foo :: (l: {number | 0 <= v}) => {void | 0 < 1} */
function foo(l?: number) {

    /*@ local loc :: number + undefined */
    let loc = l;

    if (arguments.length < 1) {

        /*@ local zero :: number + undefined */
        let zero = -1;
        loc = zero;
    }

    let l1 = <number>loc;

    assert(0 <= l1);
}
