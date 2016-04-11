
/*@ measure mul :: (number, number) => number */

/*@ multiply :: (x: number, y: number) => { v: number | v = mul x y && v = mul y x } */
declare function multiply(x: number, y: number): number;


/*@ alias pos = {number | v >  0 } */
/*@ alias nat = {number | v >= 0 } */

/*@ mulThm130 :: (a:{number | v = 130}) => {boolean | mul a a > 7100} */
declare function mulThm130(a: number): boolean;

/*@ mulThm128 :: (a:{number | v = 128}) => {boolean | mul a a < 1000000} */
declare function mulThm128(a: number): boolean;

/*@ mulThm1 :: (a:nat, c:{number | v >= 2}) => {boolean | a + a <= mul c a} */
declare function mulThm1(a: number, c: number): boolean;

/*@ mulThm2 :: (a:nat, b:number, c:{number | v >= b + 1}) => {boolean | a + (mul b a) <= mul c a} */
declare function mulThm2(a: number, b: number, c: number): boolean;

/*@ mulThm3 :: (a: nat, b: nat) => {boolean | mul a b + 1  < mul a (b + 1) }  */
declare function mulThm3(a: number, b: number): boolean;
