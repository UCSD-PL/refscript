/*@ glob :: { number | v > 0 } */
var glob = 12;


/*@ x :: { f: { number | v > 0 } } */


var x =  { f: 1 };
 
/*@ foo :: () => {void | true } */
function foo() {
    x.f = 2;    
}


/*@ main :: () => {void | true } */
function main () {
  foo();
}

