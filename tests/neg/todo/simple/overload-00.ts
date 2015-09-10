
/*@ foo :: (x: number + string) => { boolean | 0 < 1} */
function foo(x: any): boolean  {

  return 1 < x;

}
