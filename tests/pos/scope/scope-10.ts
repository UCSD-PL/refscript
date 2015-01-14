//adapted from underscore
class Foo {
  /*@ map : forall T . () : {void | true} */
  map<T>() {
    /*@ results :: Array<Mutable, T> */
    var results = [];
  }
}

// Note that it works outside of a class context:

/*@ map :: forall T . () => {void | true} */
function map<T>() {
  /*@ results :: Array<Mutable, T> */
  var results = [];
}
