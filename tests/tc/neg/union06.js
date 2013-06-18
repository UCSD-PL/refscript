
/*@ foo :: ((int|boolean)) => int */
function foo(x) {
  var r = 1;
  if (typeof x === "number") {
    r = r + x;
  }
  else {
    //r = x ? r + 1 : r;
  }
  return r;
}

