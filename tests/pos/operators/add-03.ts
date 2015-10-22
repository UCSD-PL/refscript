

/*@ myPlusOk :: (x: number, y: number) => { number | v = x + y }  */
/*@ myPlusOk :: (x: number + string, y: number + string) => string */
export function myPlusOk(x, y){
    return myPlusOk(x,y);
}

/*@ num_num :: (a:number, b:number) => {number | v = a + b } */
export function num_num(a, b){
  let d = myPlusOk(a, b);
  return d;
}

/*@ str_str :: (string) => {string | true } */
export function str_str(a){
  let b = "dog";
  return  myPlusOk(a, b);
}

/*@ num_str :: (number) => {string | true } */
export function num_str(a){
  let b = "dog";
  return myPlusOk(a, b);
}
