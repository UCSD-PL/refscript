
/*@ strConcat :: (string, string) => string */

/*@ foo :: (number | string ) => number | string */

function foo(x) {

  if (typeof(x) === "string") {
    return strConcat(x, "a");
  }
  else if (typeof(x) === "number") {
    return x + 1;
  }
  return 0;

}

