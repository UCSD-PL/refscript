
/*@ extern arr :: { [string] | (len v) = 1 } */ 

/*@ baz :: () => {v: string | true } */
function baz(){  
  return arr[0];
}

