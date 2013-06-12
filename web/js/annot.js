
'use strict';

/********************************************************************************/
/******** "Global" Annotation Table *********************************************/
/********************************************************************************/

/*@ type Annot1 = { ident : string
                  , ann   : string
                  , row   : int
                  , col   : int  
                  , size  : int
                  } */ 

/*@ type Annot  = array [array [annotJ]] */

var curAnnot = "";

/*@ annotTable :: Annot */
var annotTable 
   = { 5 : { 14 : { ident : "foo"
                  , ann   : "int -> int"
                  , row   : 5
                  , col   : 14
                  }
           }
     , 9 : { 22 : { ident : "map" 
                  , ann   : "(a -> b) -> [a] -> [b]"
                  , row   : 9
                  , col   : 22
                  }
           , 28 : { ident : "xs"
                  , ann   : "[b]" 
                  , row   : 9 
                  , col   : 28
                  }
           } 
     }

/*@ type Error = { start: pos
                 , stop : pos 
                 } 
 */

/*@ type Pos   = { line:   int
                 , column: int 
                 } 
 */


/********************************************************************************/
/******** Function Returning Annot for A Row/Column *****************************/
/********************************************************************************/

var zooper     = "   Int\n-> Bool\n-> IO String";

function getAnnotText(row, col, annT) {
  var rowA = annT[row];
  
  if (!rowA){
    // No annotations defined for this row...
    return null;
  }

  for (var c in rowA){
    if (c == col) {
      // Found annotation beginning at exact row, col
      return rowA[c].ann;
    }
  }
  return null;
}


/*@ posString :: (pos) => String */
function posString(p){
  return "(" + p.line + ", " + p.column + ")";
}

/*@ errorString :: (error) => string */
function errorString(e){
  return "Error at: " + posString(e.start) + "--" + posString(e.stop);
}

/******************************************************************/
/****** PUBLIC API ************************************************/
/******************************************************************/

/*@ annotFun :: (int, int) => string? */
function getAnnot(row, col){
  var r = getAnnotText(row + 1, col + 1, annotTable);
  if (r) { curAnnot = r;}
  return r;
}

/*@ setAnnots :: (Annot) => void */
function setAnnots(t) {
  annotTable = t;
}

/*@ getResult :: (Annot) => {v:string | v = "safe" || v = "unsafe" || v = "crash" } */
function getResult(t){
  if (!t){ 
    return "crash" 
  } else if (t.errors.length > 0) { 
    return "unsafe"
  } else { 
    return "safe" 
  }
}

/*@ getWarns :: (annot) => list [string] */
function getWarns(t){
  if (!t){ return [] }
  return t.errors.map(errorString);
}


