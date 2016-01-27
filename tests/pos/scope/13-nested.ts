
/*@ qualif AddTrhree(v:int, w: int): v = w + 3 */

/*@ foo :: () => { number | v = 4 } */
export function foo() {
    let u = 1;

    function addThree(u: number) {
        return u + 3;
    }

    return addThree(u);
}
