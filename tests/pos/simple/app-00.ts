/*@ foo :: ({number | 0 < 1}) => number */
function foo(x:number):number { 
  return x; 
}

/*@ bar :: () => void */
function bar():void { 
  foo(10);
}
