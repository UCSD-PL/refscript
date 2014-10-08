/*@ qualif Add(v:number): v = 0 */
/*@ qualif Add(v:number): v = 1 */
/*@ qualif Add(v:number): v = 2 */
/*@ qualif Add(v:number,x:number,y:number): v = x + y */
/*@ qualif Sub(v:number,x:number,y:number): v = x - y */

enum Operator {
  ADD,
  SUB,
  DIV,
  MUL
}

/*@ compute :: (o: number, a: number, b: { v: number | ((o = 2) => (v != 0))}) => 
      { number | (((o = 0) => (v = a + b)) && ((o = 1) => (v = a - b))) } */
function compute(op: Operator, a: number, b: number): number {
  if (op === Operator.ADD)
    return a + b;
  else if (op === Operator.SUB) 
    return a - b;
  else if (op === Operator.MUL) 
    return a * b;
  else if (op === Operator.DIV)
    return a / b;
  else 
    throw new Error("");
}

var opa = Operator.ADD;

assert(compute(Operator.ADD, 1, 2) === 2);
assert(compute(Operator.SUB, 2, 1) === 2);
