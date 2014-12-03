
// XXX works by removing one of the varXX definitions

var var00 = 0;
var var02 = 0;
var var03 = 0;
var var04 = 0;
var var05 = 0;
var var06 = 0;
var var07 = 0;
var var08 = 0;
var var09 = 0;
var var10 = 0;
var var11 = 0;
var var12 = 0;
var var13 = 0;
var var14 = 0;
var var15 = 0;

/*@ annVar0 :: number */
var annVar0 = 0;
/*@ annVar1 :: number */
var annVar1 = 0;

class Foo {
  x;
  /*@ y: number */
  y;
  /*@ blocks: {IArray<Block<Immutable> + null> | (len v) = 6} */
  blocks;

  /*@ new(x:number, y:number, blocks:{IArray<Block<Immutable> + null> | (len v) = 6}) => void */
  constructor(x, y, blocks) {
    this.x = x;
    this.y = y;
    this.blocks = blocks;
  }
}

class Block {
  /*@ new() => void */
  constructor() { }
}
