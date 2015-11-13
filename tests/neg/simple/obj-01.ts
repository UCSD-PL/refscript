/*@ x :: { f: { number | v > 10 } } */
var x = { f: 111 };
  
/*@ foo :: () => {void | 0 < 1 } */
function foo() {
    x.f = 2;    
}

