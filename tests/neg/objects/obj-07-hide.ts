/*@ gobj :: { a: number;
              b: string;
              oo: { m : number };
            } 
 */
var gobj = {
  a: 5,
  b: "String",
  oo: { n: 6 }
}

/*@ foo :: ({ number | true } ) => {  } */
function foo (n) {
  return gobj.oo;
}


