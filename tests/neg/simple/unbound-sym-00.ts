
/*@ outer :: (xxxx: number) => { void | 0 < 1 } */
function outer(xxxx: number) {

    /*@ foo :: (a: { number | v = xxxx }) => void */
    function foo(a: number) {  };

    /*@ bar :: (weird: number) => { number | v = zzzz } */
    function bar(weird: number) { return weird; };

}
