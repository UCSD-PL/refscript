/*@ qualif PLusOne(v: int, w: int): v = w + 1 */

/*@ gobj :: { a: number; b: string  ; oo: { n : number } } */
let gobj  = { a: 5     , b: "String", oo: { n: 6 }       };

export function foo (n:number): { n: number } {
  return gobj.oo;
}
