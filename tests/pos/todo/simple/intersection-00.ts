
/*@ foo :: ([Immutable]{ f: ({ string | v = "aaa"})=>string })=>void */
function foo(f: { f: any }) {

}

/*@ x :: {
            f: ({ string | v = "aaa"}) => string
            f: ({ number | v > 0    }) => number
        } */
declare var x;

foo(x);
