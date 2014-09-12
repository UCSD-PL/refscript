
/*@ d3_vendorSymbol :: (object: {}, name: string) => { boolean | ((Prop v) <=> keyIn(name, object)) } */
function d3_vendorSymbol(object:any, name:string) {
   return name in object;
}
