
/*@ readonnly a :: # */
var a = 1; 


/*@ foo :: () => { number | v > 0 } */
function foo():number {
  return a ;
}
