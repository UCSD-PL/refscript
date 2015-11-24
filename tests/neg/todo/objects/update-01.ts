
/*@ foo :: ({x: { v: number | v > 10 } }) => { void | 0 < 1 } */ 
function foo(o) {
  o.x = 5;
}
