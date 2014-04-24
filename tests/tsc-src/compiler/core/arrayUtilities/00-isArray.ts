
/*@ isArray :: (value: top): boolean */
function isArray(value: any): boolean {
    return Object.prototype.toString.apply(value, []) === '[object Array]';
}

