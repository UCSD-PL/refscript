
/*@ arr :: [string] */ 
var arr : string[]= [ "a" ];

/*@ baz :: () => {v: string | true } */
function baz() : string{  
  return arr[0] ;
}

