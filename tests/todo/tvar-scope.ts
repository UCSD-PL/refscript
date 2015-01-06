//adapted from underscore
class Foo {
  /*@ map : forall T . () : {void | true} */
  map<T>() {
    /*@ results :: Array<Mutable, T> */
    var results = [];
  }
}
