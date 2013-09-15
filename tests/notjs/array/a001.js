/*@ foo :: () => [| number, number, { v: number | v < 10 }, string, string, boolean |] */
function foo () {
  return [2,3,5,"hello", "world", true];
  //arr["5"];
}
