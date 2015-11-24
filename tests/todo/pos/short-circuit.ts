/*@ foo :: (arr: Array<Immutable, string>) => { boolean | 0 < 1 } */
function foo(arr) {
  if (arr.length > 0) {
    if (arr[0] === "blah") {
      return true;
    }
  }
  return false;
}

/*@ bar :: (arr: Array<Immutable, string>) => { boolean | 0 < 1 } */
function bar(arr) {
  if ((arr.length > 0) && (arr[0] === "blah")) {
    return true;
  }
  return false;
}