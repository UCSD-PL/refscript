
/*@ id :: forall A . (A) => A */
function id(a) {
  return a;
}

/*@ foo :: (number + boolean + void) => boolean + number */
function foo(x) {
  return id(x);
}

