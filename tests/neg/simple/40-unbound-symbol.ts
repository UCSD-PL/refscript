
/*@ outer :: (xxxx: number) => { void | 0 < 1 } */
function outer(xxxx: number) {

    /*@ obj :: {
                 x: [Mutable] number;
                 y: { number | v = x };
                 z: { number | v = xxxx };
               }
     */
    let obj = { x: 1, y: 2, z: 3 };

}
