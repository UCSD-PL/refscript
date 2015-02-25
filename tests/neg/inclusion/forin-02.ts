
/*@ qualif Bot(v:a,s:string): hasProperty(v,s) */
/*@ qualif Bot(v:a,s:string): enumProp(v,s) */

/*@ foo :: (o: [Immutable]{ [x:string]: string }) => { MArray<string> | true } */ 
function foo(o) {

  var ret = [];

  for (var x in o) {
    ret.push(o[x]);
  }
  ret.push(o["o"]);

  return ret;
  
};
