
/*@ foo :: <A>(x: A) => A */
export function foo(x: any): any {
    if (0 < 1) {
        return x;
    }
    return 1;
}
