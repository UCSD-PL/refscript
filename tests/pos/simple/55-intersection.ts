
/*@ foo :: (f: (Immutable) { f: (s: { string | v = "aaa" }) => string }) => void */
declare function foo(f: { f: any });

/*@ x :: (Immutable) {
            f: ({ string | v = "aaa" || v = "bbb" }) => string;
            f: ({ number | v > 0    }) => number;
        } */
declare let x;

foo(x);
