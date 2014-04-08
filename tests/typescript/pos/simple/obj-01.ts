/*@ x :: { f: { number | v > 0 } } */ 
var x = { f: 1 };
 
 
/*@ foo :: () => {void | true } */
function foo():void {
  x.f = 2;    
}

