
/*@ alias pos = {number | v >  0 } */
/*@ alias nat = {number | v >= 0 } */

/*@ mulThm130 :: (a:{number | v = 130}) => {boolean | a * a > 7100} */
declare function mulThm130(a);
/*@ mulThm128 :: (a:{number | v = 128}) => {boolean | a * a < 1000000} */
declare function mulThm128(a);
/*@ mulThm1 :: (a:nat, c:{number | v >= 2}) => {boolean | a + a <= c * a} */
declare function mulThm1(a,c);
/*@ mulThm2 :: (a:nat, b:number, c:{number | v >= b + 1}) => {boolean | a + (b * a) <= c * a} */
declare function mulThm2(a,b,c);
