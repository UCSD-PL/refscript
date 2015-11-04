/*@ option --extrainvs */

/*@ qualif HasP(v:string, s:A): hasProperty(v,s) */
/*@ qualif EnumP(v:string, s:A): enumProp(v,s)    */

/*@ foo :: (o: [Immutable]{ [x:string]: string }) => { MArray<string> | 0 < 1 } */ 
function foo(o) {

  var ret = [];

  for (var x in o) {
    ret.push(o[x]);
  }
  ret.push(o["o"]);

  return ret;
  
};
