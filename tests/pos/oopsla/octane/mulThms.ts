
/*@ option --real */

/*@ alias pos = {number | v >  0 } */
/*@ alias nat = {number | v >= 0 } */

/*@ mulThm130 :: (a:{number | v = 130}) => {boolean | a * a > 7100} */
function mulThm130(a) { return true }
/*@ mulThm128 :: (a:{number | v = 128}) => {boolean | a * a < 1000000} */
function mulThm128(a) { return true }
/*@ mulThm1 :: (a:nat, c:{number | v >= 2}) => {boolean | a + a <= c * a} */
function mulThm1(a,c) { return true }
/*@ mulThm2 :: (a:nat, b:number, c:{number | v >= b + 1}) => {boolean | a + (b * a) <= c * a} */
function mulThm2(a,b,c) {
    // assert((a * b) + a === a * (b + 1)); // either of these asserts will do
    assert(a * (b + 1) <= a * c);
    return true;
}
