
/*@ outer :: (xxxx: number) => { void | true } */
function outer(xxxx: number) {

    /*@ obj :: {
                 x: [Mutable] number;
                 y: { number | v = x };
                 z: { number | v = xxxx };
               }
     */
    var obj = { x: 1, y: 2, z: 3 };

}
