
var x /*@ { f: { number | v > 10 } } */ = { f: 111 };
  
/*@ foo :: () => {void | true } */
function foo() {
    x.f = 2;    
}

