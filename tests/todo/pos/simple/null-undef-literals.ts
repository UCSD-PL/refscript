//adapted from underscore

/*@ isNull :: <T>(ob:T) => {boolean | ((Prop v) <=> (ob ~~ null))} */
function isNull(ob) {
    return ob === null;
}

// (this one already works:)
/*@ isUndefined :: <T>(ob:T) => {boolean | ((Prop v) <=> (ob ~~ undefined))} */
function isUndefined(ob) {
    return ob === undefined;
}
