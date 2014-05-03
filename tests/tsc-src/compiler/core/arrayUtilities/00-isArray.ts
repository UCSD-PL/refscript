
/*@ isArray :: forall A . (value: A) => { boolean | true } */
function isArray(value: any): boolean {
    return Object.prototype.toString.apply(value, []) === '[object Array]';
}

