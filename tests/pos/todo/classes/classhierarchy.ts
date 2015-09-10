//adapted from transducers
declare class Super {}
declare class Sub extends Super {}
/*@ x :: {Sub<Immutable> | extends_class (v,"Super")} */
var x = new Sub();
