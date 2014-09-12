
/*@ d3_vendorSymbol :: (object: {}, name: string) => { boolean | ((Prop v) <=> keyIn(name, object)) } */
function d3_vendorSymbol(object:any, name:string) {

   return name in object;

}

// var d3_vendorPrefixes = ["webkit", "ms", "moz", "Moz", "o", "O"];
