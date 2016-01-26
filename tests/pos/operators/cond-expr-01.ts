//adapted from transducers

/*@ foo :: () => { number | v = 4 } */
export function foo() {
    return (true ? 3 : null) + 1;
}
