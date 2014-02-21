
/*@ extern arr :: { [string] | (len v) = 1 } */ 

/*@ baz :: () => {v: string | true } */
function baz() : string{  
  return arr[0];
}

