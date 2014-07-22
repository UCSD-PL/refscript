
/*@ foo :: () => { void | true } */ 

function foo() {
  
  /*@ gggg :: { number | v > 0  } */
  var gggg = 1;

  gggg = gggg + 1;

}

/*@ bar :: () => { number | v = 2 } */ 
function bar() {

  var s = 1;
  
  /*@ gggg :: { string | v = "a" } */
  var gggg = "a";

  s = s + 1;

  gggg = "a";

  return s;

}

