
declare function foo_47(x: Array<Unique, number>, y: Array<Unique, number>): void;

let a_47 = [1, 3];
foo_47(a_47, a_47);         // FAIL


declare function bar_47(x: Array<Mutable, number>): void;

let b_47 = [1, 2];
bar_47(b_47);               // OK

// equivalent

bar_47(consume(b_47));

//
// consume :: (G, a: immutable number[]) => (remInuqe(G,a), p number[])   where p is any permission
//
