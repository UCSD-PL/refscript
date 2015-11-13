/*@ map :: /\ forall T . (string) => {void | 0 < 1}
           /\ forall T . (number) => {void | 0 < 1} */
function map<T>(x) {
  /*@ results :: IArray<T> */
  var results = [];
}
