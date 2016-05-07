/*@ myArray :: { IArray<string> | (len v) = 1 } */
declare let myArray: string[];


assert(typeof myArray[0] === "string"); 


