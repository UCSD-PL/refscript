/*@ myArray :: { IArray<string> | (len v) = 1 } */
declare var myArray: string[];


assert(typeof myArray[0] === "string"); 


