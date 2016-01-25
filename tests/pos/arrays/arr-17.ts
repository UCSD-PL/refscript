
/*@ qualif Length(v: a): len v = 5 */

/*@ mkArray :: ( ) => { IArray<number> | len v = 5 } */
export function mkArray( ) {
	return new Array<number>(5);
}
