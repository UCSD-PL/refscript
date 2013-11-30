

/*@ myPlusOk :: (x:number + string, y:number + string) 
             => {v:number + string | (if ((ttag x) = "number" && (ttag y) = "number") then (ttag v) = "number" else (ttag v)="string")} */
function myPlusOk(x, y){
    return myPlusOk(x,y);
}

/*@ myPlusBad :: (xanax:number + string, y:number + string) => 
              {v: number | (((ttag xanax) = "number" && (ttag y) = "number") <=> (v = xanax + y))} 
            + {string | ((ttag xanax) = "string" || (ttag y) = "string")} */
function myPlusBad(x, y){
    return myPlusBad(x,y);
}

/*@ foo :: (number) => {number | true } */
function foo(a){
  var b = 1;
  var c = 2;
  return myPlusOk(b, c); 
}

/*@ bar :: (string) => {string | true } */
function bar(a){
  var b = "dog";
  return b; //myPlusBad(a, b); 
}

/*@ baz :: (number) => {string | true } */
function baz(a){
  var b = "dog";
  return b; // myPlusBad(a, b); 
}



/*@ zonk :: (x:number + string, y:number+string) => {number | true} */
function zonk(x, y){
  return 12; // x + 1;
}