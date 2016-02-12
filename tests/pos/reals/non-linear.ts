
/* option "--real" */

/*@ alias nat = {number | v >= 0} */

/*@ foo :: (x:nat, y:nat) => {void | 0 < 1} */

function foo(x,y) {

  assert((x+1)+(y+1)*(x+2) < (x+2)*(y+2));

}
