
/*  foo :: (f: () => number) => number */
/*@ foo :: (f: number) => number */
export function foo(f) {
    if (typeof f === "function") {
        return f + 1;
    }
    else {
        // assert(false);
        return f();
    }
}
