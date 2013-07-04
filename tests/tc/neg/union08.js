
/*@ id :: forall A . (A,A) => A */
function id(a,b) {
  return a;
}

/*@ foo :: (number|boolean) => number|boolean */
function foo(x) {
  return id(x,1);
}

