/*@ qualif PLusOne(v:number, w: number)     : v = w + 1                            */

/*@ inc :: (number) => number  */
function inc(n) { return n + 1; }

/*@ readonly */ let obj = {
  a: 5,
  b: "String",
  oo: { n: 6 }
};

/*@ foo :: (number) => { b: boolean } */
export function foo (n) {
    return obj.oo;
}
