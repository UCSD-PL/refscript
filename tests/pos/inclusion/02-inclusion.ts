
/*@ d3_vendorSymbol :: (object: {}, name: string) => { boolean | ((Prop v) <=> hasProperty(object, name)) } */
function d3_vendorSymbol(object:any, name:string) {
    return name in object;
}
