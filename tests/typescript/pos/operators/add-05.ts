
/*@ myPlusOk :: (x:number + string, y:number + string) 
             => {number | ((ttag x) = "number" && (ttag y) = "number" && v = x + y) } + 
                {string | ((ttag x) = "string" || (ttag y) = "string") } 
                      
  */
function myPlusOk(x, y){
    return myPlusOk(x,y);    
}


/*@ num_num :: (a:number, b:number) => {number | v = a + b } */
function num_num(a:number, b:number){
	var d :number= myPlusOk(a, b);
	return d; 
}

/*@ str_str :: (string) => {string | true } */
function str_str(a:string):string{
	var b:string = "dog";
	return  myPlusOk(a, b); 
}

/*@ num_str :: (number) => {string | true } */
function num_str(a:number):string{
	var b:string = "dog";
	return myPlusOk(a, b); 
}



