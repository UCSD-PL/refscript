
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

//function createElt(tagName: "div"): HTMLDivElt;
//function createElt(tagName: "span"): HTMLSpanElt;
//function createElt(tagName: "canvas"): HTMLCanvasElt;

/*@ predicate TT v x n e = (x = n => instanceof(v, e)) */

/*@ createElt :: (tagName: string) => {v: HTML |  TT(v, tagName, "div", "DivElt")
                                               && TT(v, tagName, "span", "SpanElt")
                                               && TT(v, tagName, "canvas", "CanvasElt") } */

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

