
// TODO : add explicit cast here.

/*@ foo :: (p: #Point + { x: string } ) => { top | true }  */
function foo(p: any) {

  return p.x;

}
