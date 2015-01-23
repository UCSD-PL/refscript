
/*@ qualif Bot(v:a,s:string): hasProperty(v,s) */
/*@ qualif Bot(v:a,s:string): enumProp(v,s) */

/*@ foo :: (o: [#Immutable]{ [x:string]: string }) => { #Array[#Mutable, string] | true } */ 
function foo(o) {

  var ret = [];

  for (var x in o) {
    ret.push(o[x]);
  }  

  return ret;
  
};
