
class HTMLElt { 

  constructor() { }
}

class HTMLDivElt extends HTMLElt { 
  constructor() { super(); }
}

/*@ foo :: (elt: { HTMLElt<Immutable> | true } ) => HTMLDivElt<Immutable> */
function foo(elt: HTMLElt): HTMLDivElt {
  if (elt instanceof HTMLCanvasElt) {
    var div = <HTMLDivElt>elt;
    return div;
  }
  throw new Error("");
}

class HTMLCanvasElt extends HTMLElt { 
  constructor() { super(); }
}
