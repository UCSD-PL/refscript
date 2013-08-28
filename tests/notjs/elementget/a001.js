var obj = {f1: "first", f2: "second"};


/*@ foo :: () => { string | (v = "first") } */
function foo () {
  return obj["f1"];
}
