
/*@ opt :: { f?: [Mutable] {number | v > 0 } } */
var opt: { f?: number } = { } ;

opt.f = -1;

