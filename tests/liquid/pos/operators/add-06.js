
/*@ myPlusOk :: (x:number + string, y:number + string) 
             => {number | ((ttag x) = "number" && (ttag y) = "number" && v = x + y) } + 
                {string | ((ttag x) = "string" || (ttag y) = "string") } 
                      
  */
function myPlusOk(x, y){
    return myPlusOk(x,y);    
}

/*@ num_one :: (a:number) => void */
function num_one(a){
  var d = myPlusOk(0, 1);
  assert (d == 1);
  return; 
}



