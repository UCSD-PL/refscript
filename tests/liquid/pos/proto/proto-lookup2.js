/*@ foo :: ({ __proto__ : { x: boolean }, x: number}) => { number | true } */
function foo(o) {
    return o.x;
}
