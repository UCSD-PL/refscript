/*@ id :: forall A . (A,A) => A */
function id(a,b) {
  return b;
}

/*@ foo :: (number|boolean) => number */
function foo(x) {
  return id(x,1);
}

