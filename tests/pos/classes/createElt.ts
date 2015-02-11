
/*@ alias HTML = HTMLElt<Mutable> */

class HTMLElt {
  constructor() { }
}

class DivElt extends HTMLElt {  
  public htmlDivElt__ = 0; 
  constructor() {
    super();
  }
}

class SpanElt extends HTMLElt { 
  public htmlSpanElt__ = 1; 
  constructor() { super(); }
}

class CanvasElt extends HTMLElt { 
  public htmlCanvasElt__ = 2; 
  constructor() { super(); }
}

//function createElt(tagName: "div"): HTMLDivElt;
//function createElt(tagName: "span"): HTMLSpanElt;
//function createElt(tagName: "canvas"): HTMLCanvasElt;

/*@ predicate TT v x n e = (x = n => extends_class(v, e)) */

/*@ createElt :: (tagName: string) => {v: HTML |  TT(v, tagName, "div", "DivElt")
                                               && TT(v, tagName, "span", "SpanElt")
                                               && TT(v, tagName, "canvas", "CanvasElt") } */

function createElt(tagName: string): HTMLElt {
 
  /*@ res :: HTML */
  var res:HTMLElt;
 
  if (tagName === "div")
      res = new DivElt();

  else if (tagName === "span")
      res = new SpanElt();

  else if (tagName === "canvas")
      res = new CanvasElt();
  
  else
      res = new HTMLElt();

  return res;

}

var elt = createElt("div");

assert(elt instanceof DivElt);

