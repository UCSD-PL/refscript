
/*@ a :: { v: bitvector32 | v = lit "#x00000001" (BitVec (Size32 obj)) } */
var a = 0x00000001;


if (a) {
  assert(true);
}
else {
  assert(false);
}

