var x = /*: { f: { number | true} } */ { f: 1 };
 
/*@ foo :: () => {void | true } */
function foo() {
    x.f = 2;    
}


/*@ main :: () => {void | true } */
function main () {
  foo();
}

