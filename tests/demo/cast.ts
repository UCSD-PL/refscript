
/*@ foo :: () => number + undefined */
declare function foo(): any;

export function bar(): number {
    let x = foo();
    if (x) { // Try removing this undefined check
        return 1 + <number>x;
    }
    return 0;
}
