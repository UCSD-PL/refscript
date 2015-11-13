//adapted from underscore
class Foo {
  constructor() {}

  /*@ map : forall T . () : {void | 0 < 1} */
  map<T>() {
    /*@ results :: Array<Mutable, T> */
    var results = [];
  }
}

// Note that it works outside of a class context:

/*@ map :: forall T . () => {void | 0 < 1} */
function map<T>() {
  /*@ results :: Array<Mutable, T> */
  var results = [];
}
