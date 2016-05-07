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
  let d = myPlusOk(0, 1);
  assert (d === 2);
  return d; 
}

/*@ num_one :: (a:number) => {number | v = a + 1} */
function num_one(a){
  let d = myPlusOk(a, 1);
  return d; 
}

