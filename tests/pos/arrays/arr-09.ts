/*@ arr :: { #Array[#Immutable,string] | (len v) = 1 } */ 
declare var arr : string[]; 

/*@ baz :: () => {v: string | true } */
function baz() : string{  
  return arr[0];
}

