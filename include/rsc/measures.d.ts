
/*@ measure hasProperty         :: <A>  (x: A, p: string) => bool */
/*@ measure hasDirectProperty   :: <A>  (x: A, p: string) => bool */
/*@ measure enumProp            :: <A>  (x: A, p: string) => bool */
/*@ measure ttag                :: <A>  (x: A) => string          */
/*@ measure Prop                :: <A>  (x: A) => bool            */
/*@ measure extends_class       :: <A>  (x: A, c: string) => bool */
/*@ measure extends_interface   :: <A>  (x: A, i: string) => bool */
/*@ measure offset              :: <A,B>(x: A, y: string) => B    */
/*@ measure len                 :: <M,A>(x: Array<M,A>) => number */
