
var arr /*@ readonly */ : string[] = [ "a" ];

/*@ baz :: () => string */
function baz() : string {  
  return arr[0] ;
}

baz();
