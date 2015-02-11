/*@ alias HTML = HTMLElt<Mutable> */


class HTMLElt { 

  constructor() { }
}

class DivElt extends HTMLElt {  
  public htmlDivElt__ = 0; 
  constructor() { super(); }
}

class SpanElt extends HTMLElt { 
  public htmlSpanElt__ = 1; 
  constructor() { super(); }
}

class CanvasElt extends HTMLElt { 
  public htmlCanvasElt__ = 2; 
  constructor() { super(); }
}

// UNCHECKED TS Specification
//
// function createElt(tagName: "div"): HTMLDivElt;
// function createElt(tagName: "span"): HTMLSpanElt;
// function createElt(tagName: "canvas"): HTMLCanvasElt;


/*@ predicate TT x n v t = (x = n => extends_class(v, t)) */

/*@ alias HTMLX<x> = {v: HTML | TT(x, "div"   , v, "DivElt") &&
                                TT(x, "span"  , v, "SpanElt") &&
                                TT(x, "canvas", v, "CanvasElt") } */

/*@ createElt :: (tagName: string) => HTMLX<tagName>  */
function createElt(tagName: string): HTMLElt {
  
  /*@ res :: HTML */
  var res;

  if (tagName === "div")    
      res = new DivElt();
  
  else if (tagName === "span")   
      res = new SpanElt();
  
  else if (tagName === "canvas") 
      res = new CanvasElt();
  
  else res = new HTMLElt();
  
  return res;
  
}

var elt = createElt("div");

assert(elt instanceof DivElt);

