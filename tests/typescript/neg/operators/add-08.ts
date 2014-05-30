/*@ poo :: (number) => number */
function poo(x){
  return 0;
}

/*@ myPlusOk :: /\ (x:number, y:number) => {number | v = x + y}
                /\ (x:number, y:string) => string
                /\ (x:string, y:number) => string
                /\ (x:string, y:string) => string
  */
function myPlusOk(x, y){
    return myPlusOk(x, y);    
}

/*@ one :: () => {number | v = 1} */
function one(){
  var d = myPlusOk(0, 1);
  assert (d === 1);
  return d; 
}

/*@ num_one :: (a:number) => {number | v = a + 1} */
function num_one(a){
  var d = myPlusOk(a, 1);
  return d; 
}

/*@ num_str :: (a:number) => {number | true} */
function num_str(a){
  var d = myPlusOk(0, "cat");
  return d; 
}

