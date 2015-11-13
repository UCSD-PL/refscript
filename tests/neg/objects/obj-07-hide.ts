/*@ gobj :: { a: {v:number|0 < 1};
              b: {v:string|0 < 1};
              oo: {m : {v:number|0 < 1} };
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


