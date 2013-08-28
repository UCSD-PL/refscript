var obj = {f1: "hello ", f2: "world"};

var str = "f2";



/*@ foo :: () => { string | true } */
function foo () {
  return obj["f1"] + obj[str];
}
