/*@ qualif PLusOne(v:number, w: number)     : v = w + 1                            */

var gobj 
/*@ { a: number 
    , b: string
    , oo: { n : number }
    } 
 */
= {
  a: 5,
  b: "String",
  oo: { n: 6 }
};

/*@ foo :: ({ number | true } ) => { n:number } */
function foo (n:number):Object {
  return gobj.oo;
}


