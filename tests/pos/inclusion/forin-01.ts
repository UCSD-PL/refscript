
/*@ option --extrainvs */

// Taken from strobe
/*@ qualif HasP(v:string, s:A): hasProperty(v,s) */
/*@ qualif EnumP(v:string, s:A): enumProp(v,s)    */

/*@ foo :: (o: { [x:string]: string + number }) => { string | true } */ 
function foo(o) {
  for (var x in o) {
    var r = o[x];
    if (typeof r === "string") {
      return r;
    }
  }
  return "no string found";
};
