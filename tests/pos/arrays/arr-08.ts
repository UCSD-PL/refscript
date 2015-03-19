
/*@ readonly arr :: # */
var arr = [ "a" ];

/*@ baz :: () => string */
function baz() : string {  
  return arr[0] ;
}

baz();
