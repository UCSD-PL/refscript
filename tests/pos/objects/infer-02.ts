var gobj= {
  a: 5,
  b: "String",
  oo: { n: 6 }
};


/*@ foo :: ({ number | true } ) => { n:number } */
function foo (n:Object) :Object{
  return gobj.oo;
}


