

class Point { 
  /*@ x : number */ 
  x: number = 1; 

  /*@ y : number */ 
  y: number = 2
}  

assert("x" in new Point());  // returns true
assert("y" in new Point());  // returns true
