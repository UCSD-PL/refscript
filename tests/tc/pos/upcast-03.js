

/*@ id :: forall A. (a: A) => A */
function id(a) { 
  return a;
}


/*@ simple :: (x: {a: number}) => {  } */
function simple(x) {
  return id(x);
}


