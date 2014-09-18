/*@ fails :: (number) => {number + undefined | true} */
function fails (x:number):any { 
    return x ? true : undefined;
}

/*@ ok1 :: (number) => {number + undefined | true} */
function ok1(x:number):any { 
    if (x) {return 10;} else {return undefined;}
}

/*@ ok2 :: (number) => {number + undefined | true} */
function ok2(x:number):any { 
    /*@ z :: number + undefined */
    var z;
    if (x) {z = 10;} else {z = undefined;}
    return z;
}

