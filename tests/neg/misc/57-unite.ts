/*@ foo :: <A,B>(A, B) => A */
export function foo(x: any, y: any) {
    if (x === y) {
        return x;
    } else {
        return y;
    }
}
