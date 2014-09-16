
/*@ qualif Bot(v:a,s:string): keyIn(v,s) */

/*@ foo :: (o: [#Immutable]{ [x:string]: string }) => { #Array[#Mutable, string] | true } */ 
function foo(o) {

  var ret = [];

  for (var x in o) {
    var aaa = o[x];
    ret.push(aaa);
  }  
  ret.push(o["o"]);

  return ret;
  
};
