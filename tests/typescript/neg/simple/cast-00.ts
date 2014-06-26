


class HTMLElt { }


class HTMLDivElt extends HTMLElt {

  /*@ new()=>void */
  constructor() {
    super();
  }

  /*@ htmlDivElt__ : number */
  public htmlDivElt__: number;

}

class HTMLSpanElt extends HTMLElt {

  /*@  htmlSpanElt__ : number */
  public htmlSpanElt__: number;

}

class HTMLCanvasElt extends HTMLElt {

  /*@ htmlCanvasElt__ : number */
  public htmlCanvasElt__: number;

}


/*@ foo :: (elt: { #HTMLElt | true } ) => #HTMLDivElt */
function foo(elt: HTMLElt): HTMLDivElt {

  if (elt instanceof HTMLCanvasElt) {
    var div = <HTMLDivElt>elt;
    return div;
  }

  throw new Error("");

}