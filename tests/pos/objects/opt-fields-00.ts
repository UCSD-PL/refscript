
/*@ opt :: { f? : number } */
declare var opt: { f?: number };

/*@ man :: { f: number } */
declare var man: { f: number };

opt = man;
