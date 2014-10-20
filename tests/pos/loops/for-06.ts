
/*@ foo :: () => { void | true } */
function foo():void { 
  for (var x:number = 1; x < 3; x++) { }
}

/*@ bar :: () => { void | true } */
function bar():void {
  var x:number;
  for (x = 1; x < 3; x++) { }
  assert(x >= 3);
}
