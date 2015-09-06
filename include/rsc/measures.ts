
/**
 * 	hasProperty: this property is true if the first string argument is a
 * 	property of the object referenced in the second, INCLUDING prototype traversal.
 *
 *  hasDirectProperty: this property is true if the first string argument is a
 *  properyty of the object referenced in the second, NOT INCLUDING prototype
 *  traversal.
 */

/*@ measure hasProperty         :: forall A   . (string, A) => bool */
/*@ measure hasDirectProperty   :: forall A   . (string, A) => bool */
/*@ measure enumProp            :: forall A   . (string, A) => bool */
/*@ measure ttag                :: forall A   . (A) => string */
/*@ measure Prop                :: forall A   . (A) => bool */
/*@ measure extends_class       :: forall A   . (A,string) => bool */
/*@ measure extends_interface   :: forall A   . (A,string) => bool */
/*@ measure offset              :: forall A B . (x:A, y:string) => B */
