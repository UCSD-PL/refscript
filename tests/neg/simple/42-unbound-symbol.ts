
/*@ outer :: (xxxx: number) => void */
export function outer(xxxx: number) {

    /*@ foo :: (a: { number | v = xxxx }) => void */
    function foo(a: number) {

    };

    /*@ bar :: (weird: number) => { number | v = zzzz } */
    function bar(weird: number) {
        return weird;
    };

}
