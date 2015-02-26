
interface IPoint {  
  x: number;
  y: number;
}

interface ICPoint {
  c: string;
}

class P implements IPoint {
  public x: number;
  public y: number;

  constructor(x: number) { this.x = x; this.y = x; }
}

class CP extends P implements ICPoint {
  /*@ c : { string | v = "red" } */
  public c: string;
   
  /*@ new (c: { string | v = "red" }) => void */
  constructor (c: string) { super(1); this.c = c; }
}

class RCP extends CP {
	constructor() { super("red"); }
}
