
/*@ foo :: ([#Mutable]{x: number}) => {void | true} */ 
function foo(o):void {
  o.x = 10;
}
