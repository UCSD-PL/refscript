/*@ map :: /\ forall T . (string) => {void | true}
           /\ forall T . (number) => {void | true} */
function map<T>(x) {
  /*@ results :: IArray<T> */
  var results = [];
}
