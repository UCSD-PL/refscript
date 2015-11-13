
/*@ myPlusOk :: /\ (x: number, y: number) => { number | v = x + y }  
                /\ (x: number + string, y: number + string) => string */
function myPlusOk(x, y){
    return myPlusOk(x,y);    
}

/*@ assertEqual :: (x:number, y:number) => void */ 
function assertEqual(x:number, y:number) {
  assert(x===y);
}

/*@ num_one :: (a:number) => {void | 0 < 1} */
function num_one(a:number):void{
  var d = myPlusOk(0, 1);
  assertEqual(d, 1);
  return; 
}



