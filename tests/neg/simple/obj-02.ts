
// TODO : add explicit cast here.

/*@ foo :: (p: #Point + { x: string } ) => { top | 0 < 1 }  */
function foo(p: any) {

  return p.x;

}
