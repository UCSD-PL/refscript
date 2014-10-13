
class HTMLElt { }
class HTMLDivElt extends HTMLElt {  public htmlDivElt__ = 0; }
class HTMLSpanElt extends HTMLElt { public htmlSpanElt__ = 1; }
class HTMLCanvasElt extends HTMLElt { public htmlCanvasElt__ = 2; }

//function createElt(tagName: "div"): HTMLDivElt;
//function createElt(tagName: "span"): HTMLSpanElt;
//function createElt(tagName: "canvas"): HTMLCanvasElt;

/*@ createElt :: (tagName: string) => { v: HTMLElt<Mutable> | (((tagName = "div")    => instanceof(v,"HTMLDivElt"))    && 
                                                               ((tagName = "span")   => instanceof(v,"HTMLSpanElt"))   &&
                                                               ((tagName = "canvas") => instanceof(v,"HTMLCanvasElt")) 
                                                              ) }  */
function createElt(tagName: string): HTMLElt {
  if      (tagName === "div")    return new HTMLDivElt();
  else if (tagName === "span")   return new HTMLSpanElt();
  else if (tagName === "canvas") return new HTMLCanvasElt();
  else return new HTMLElt();
}

var elt = createElt("div");
assert(elt instanceof HTMLDivElt);

