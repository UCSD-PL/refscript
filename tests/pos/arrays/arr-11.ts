//XXX: WORKS with this line - some qualifier scraping is missing...
/*@ qualif Length(v:number): (len v) = 3 */

/*@ foo :: () => {void | true} */
function foo(){

  /*@ a :: IArray<number> */
  var a = [1,2,3];

  /*@ b :: { IArray<number> | len v = 4 } */
  var b = [1,2,3,4];
 
  assert(a.length + b.length === 7);
  
  return;

}
