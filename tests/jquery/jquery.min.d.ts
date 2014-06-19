/**
 * The jQuery instance members
 */
interface JQuery {
  each      : any;
  css       : any;

  parent    : ()       => JQuery ;
  // parent    : (string) => JQuery ;

  children  : ()       => JQuery ;
  // children  : (string) => JQuery ;

  next      : () => JQuery;
  prev      : () => JQuery;
}

/* How
 * 
 *
 *
 */




/* 
Stream
  Tweet+
    Time
    Author
    Content
*/

/* 
interface JQuery {
  each : ∀ e <: Element . [’jqh0+h’eii] (’e -> Undef) -> ’jqh’mi
  css : ∀ e <: Element . (  [’jqh1h’eii] Str -> Str 
                          & [’jqh1+h’eii] Str*Str -> ’jqh1+h’eii)
} 
*/



