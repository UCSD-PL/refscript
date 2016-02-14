
/*@ foo :: <A>(A, A) => A*/
export function foo(x, y) {
    if (x === y) {
        return x;
    } else {
        return y;
    }
}

/*@ bar :: <A>(A, B) => {v:A | 0 < 1} */
function bar(x, y) {
    return foo(x, y);
}
