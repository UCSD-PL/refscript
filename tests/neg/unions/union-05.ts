function noop(u) {}

/*@ foo :: (u:boolean + null) => {number | 0 < 1} */
function foo(u) {
  noop(u); 
  return "not a number"; // UNSAFE
}
