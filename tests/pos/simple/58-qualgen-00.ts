
/*@ qualif AA (v: int): (v < 3) */
/*@ qualif AA (v: a): (len v = 2) */

let junk = 0;

/*@ readonly arr :: IArray<number> */
let arr = [1, 2];


module A {

    /*@ foo :: () => { number | v < 4 } */
    export function foo(): number {
        // arr[0] = 2;
        let v = arr[0];
        return v + 1;
    }

}
