
/*@ foo :: () => { v: number | v = 3 } */
function foo () {
  var obj = {f1: {f11: 1} };
  
  // OK
  // return obj.f1.f11 + obj.f1.f11 + obj.f1.f11;

  // BAD: 
  return obj["f1"].f11 + obj.f1["f11"] + obj["f1"]["f11"];
}
