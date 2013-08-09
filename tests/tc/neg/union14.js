// This is not allowed and appears as error in unification: join of substs


/*@ id :: forall A . (A,A) => A */
function id(a,b) {
  return b;
}

/*@ foo :: (number+ boolean) => number + boolean */
function foo(x) {
  return id(1,x);
}

