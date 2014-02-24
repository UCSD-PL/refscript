
/*@ foo :: ({x: { v: number | v > 10 } }) => { void | true } */ 
function foo(o) {
  o.x = 5;
}
