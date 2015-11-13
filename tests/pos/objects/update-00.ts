
/*@ foo :: ([#Mutable]{x: [#Mutable] number}) => {void | 0 < 1} */ 
function foo(o):void {
  o.x = 10;
}
