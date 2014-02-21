/*@ foo :: ({number | true}) => number */
function foo(x) { 
  return x; 
}

/*@ bar :: () => void */
function bar() { 
  foo(10);
}
