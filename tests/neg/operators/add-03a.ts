

/*@ myPlusOk :: /\ (x: number, y: number) => { number | v = x + y }  
                /\ (x: string, y: string) => { string | true      } */
function myPlusOk(x, y){
    return myPlusOk(x,y);
}

/*@ num_num :: (a:number, b:number) => {number | v = a + 1 } */
function num_num(a, b){
  let d = myPlusOk(a, b);
  return d; 
}
