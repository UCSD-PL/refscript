
/*@ foo :: (o: [#Immutable]{ [x:string]: string }) => { #Array[#Mutable, string] | true } */ 
function foo(o) {

  var ret = [];

  for (var x in o) {
    ret.push(o[x]);
  }  

  return ret;
  
};
