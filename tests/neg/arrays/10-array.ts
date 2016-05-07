//XXX: WORKS with this line - some qualifier scraping is missing...
/*@ qualif Length(v:number): (len v) = 3 */

/*@ foo :: () => {void | 0 < 1} */
export function foo(){
  let a: IArray<number> = [1,2,3];
  /*@ b :: { IArray<number> | len v = 4 } */
  let b = [1,2,3,4];
  assert(a.length + b.length === 7);
  return;
}
