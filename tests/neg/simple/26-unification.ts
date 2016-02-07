
export declare function ff(): number;

/*@ foo :: <A>(x: A) => A */
export function foo(x: any): any {
    if (0 < ff()) {
        return x;
    }
    return 1;
}
