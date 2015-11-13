
/*@ bar :: (x: string) => { void | 0 < 1 } */
declare function bar(x: string): void;

bar("");


function foo<V,U,W>(x: V, f: (v: U) => W, a: U): W {
  return f.call(x, a);
}

foo(["a"], bar, "b");
