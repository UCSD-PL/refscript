
/*@ qualif Ineq(v : int): (v <= 6) */

/*@ loop :: () => { number | v = 6 } */
// function loop() {
//     let x = 1;
//     while (x <= 5) {
//         x = x + 1;
//     }
//     return x;
// }


function loop() {
    let x;
    if (0 < 5) {
        x = 6;
    }
    return x;
}
