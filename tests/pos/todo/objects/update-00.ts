
/*@ foo :: ([#Mutable]{x: [#Mutable] number}) => {void | true} */ 
function foo(o):void {
  o.x = 10;
}
