

class Point { 
  /*@ x : number */ 
  x: number; 

  /*@ y : number */ 
  y: number 
}  

assert("x" in new Point());  // returns true
assert("z" in new Point());  // returns true
