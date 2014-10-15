/*@ alias HTML = HTMLElt<Mutable> */


class HTMLElt { }

class DivElt extends HTMLElt {  
  public htmlDivElt__ = 0; 
}

class SpanElt extends HTMLElt { 
  public htmlSpanElt__ = 1; 
}

class CanvasElt extends HTMLElt { 
  public htmlCanvasElt__ = 2; 
}

// UNCHECKED TS Specification
//
// function createElt(tagName: "div"): HTMLDivElt;
// function createElt(tagName: "span"): HTMLSpanElt;
// function createElt(tagName: "canvas"): HTMLCanvasElt;



/*@ predicate TT x n v t = (x = n => instanceof(v, t)) */

/*@ alias HTMLX<x> = {v: HTML | TT(x, "div"   , v, "DivElt") &&
                                TT(x, "span"  , v, "SpanElt") &&
                                TT(x, "canvas", v, "CanvasElt") } */

/*@ createElt :: (tagName: string) => HTMLX<tagName>  */
function createElt(tagName: string): HTMLElt {
  
  if (tagName === "div")    
      return new DivElt();
  
  else if (tagName === "span")   
      return new SpanElt();
  
  else if (tagName === "canvas") 
      return new CanvasElt();
  
  else return new HTMLElt();
  
}

var elt = createElt("div");
assert(elt instanceof DivElt);
