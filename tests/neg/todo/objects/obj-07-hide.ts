/*@ gobj :: { a: {v:number|true};
              b: {v:string|true};
              oo: {m : {v:number|true} };
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


