
/**
 * 	hasProperty: this property is true if the first string argument is a
 * 	property of the object referenced in the second, INCLUDING prototype traversal.
 *
 *  hasDirectProperty: this property is true if the first string argument is a
 *  properyty of the object referenced in the second, NOT INCLUDING prototype
 *  traversal.
 */

/*@ measure hasProperty         :: <A>  (string, A) => bool     */
/*@ measure hasDirectProperty   :: <A>  (string, A) => bool     */
/*@ measure enumProp            :: <A>  (string, A) => bool     */
/*@ measure ttag                :: <A>  (A) => string           */
/*@ measure Prop                :: <A>  (A) => bool             */
/*@ measure extends_class       :: <A>  (A,string) => bool      */
/*@ measure extends_interface   :: <A>  (A,string) => bool      */
/*@ measure offset              :: <A,B>(x:A, y:string) => B    */
/*@ measure len                 :: <M,A>(Array<M,A>) => number  */
