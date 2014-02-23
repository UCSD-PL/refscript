declare var ext_arr: string[];
/*@ extern ext_array :: { [string] | (len v) = 1 } */

/*@ extern ext_fun :: () => { [string] | (len v) = 1 } */

/*@ foo :: () => string */
function foo():string {
  
  return ext_array[0];
//  return ext_fun()[0];
  
}

