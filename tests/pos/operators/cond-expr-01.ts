//adapted from transducers

/*@ foo :: () => { number | v = 4 } */
export function foo() {
    return (<number>(true ? 3 : null)) + 1;
}
