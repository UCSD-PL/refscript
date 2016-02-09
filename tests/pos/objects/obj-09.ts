
/*@ qualif Eq1(v: int): v = 1 */

/*@ local obj :: {
    x: [Immutable] number;
    y:             { number | v = offset(this, "x") };
  } */
var obj = { x: 1, y: 1 };
