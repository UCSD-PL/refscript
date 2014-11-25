
class HTMLElt { }

class HTMLDivElt extends HTMLElt {
  /*@ new()=>void */
  constructor() {
    super();
  }

  /*@ htmlDivElt__ : number */
  public htmlDivElt__: number = 0;
}

/*@ foo :: (elt: { HTMLElt<Immutable> | true } ) => HTMLDivElt<Immutable> */
function foo(elt: HTMLElt): HTMLDivElt {
  if (elt instanceof HTMLDivElt) {
    var div = <HTMLDivElt>elt;
    return div;
  }
  throw new Error("");
}
