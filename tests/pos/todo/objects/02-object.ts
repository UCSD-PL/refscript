/*@ qualif PLusOne(v:number, w: number)     : v = w + 1                            */

/*@ gobj :: { a: number; b: string  ; oo: { n : number } } */
var gobj  = { a: 5     , b: "String", oo: { n: 6 }       };

/*@ foo :: ({ number | 0 < 1 } ) => { n:number } */
function foo (n:number):Object {
  return gobj.oo;
}


