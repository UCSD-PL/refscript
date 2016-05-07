/*@ foo :: ({ __proto__ : { x: boolean }, x: number}) => { number | 0 < 1 } */
function foo(o) {
    return o.x;
}
