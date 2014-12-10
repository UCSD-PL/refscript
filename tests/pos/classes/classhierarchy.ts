//adapted from transducers
declare class Super {}
declare class Sub extends Super {}
/*@ x :: {Sub<Immutable> | instanceof (v,"Super")} */
var x = new Sub();
