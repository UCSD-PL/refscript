

class Point { 
  x: number = 1;
  y: number = 2;
  constructor () {}
}  

assert("x" in new Point());  // returns true
assert("y" in new Point());  // returns true
