interface Term {
  /*@ tag : { number | ((0 <= v) && (v <= 2))} */
  tag : number;
  isValue: boolean;
  /*@ replace: forall T . (v: #Term, t: T): T */
  replace<T extends Term>(v: Term, t: T): T;
  /*@ reduce: (): #Term */
  reduce(): Term;
  /*@ toString: (): string */
  toString(): string;
}

class Var implements Term {
  /*@ tag : { number | v = 0 } */ 
  tag : number = 0;
  /*@ isValue : { boolean | ~Prop(v) } */
  isValue: boolean = false;
  /*@ new
  constructor(public name: string) {  }
  replace(v: Var, t: Term): Term  { return (this.name === v.name) ? t: this; }
  reduce(): Term { alert("STUCK!"); return null; }  
  toString(): string { return this.name; }
}
  
// class Lam implements Term {
//   /*@ tag :: { number | v = 1 } */ 
//   tag : number = 1;
//   /*@ isValue :: { boolean | Prop(v) } */ 
//   isValue: boolean = true;
//   constructor(public Var: Var, public term: Term) { }
//   
//   replace(v: Var, t: Term): Term {
//     //XXX: Capture ignored
//     return new Lam(this.Var, this.term.replace(v,t));
//   }
//   reduce(): Term { return this; }
//   toString(): string { return "λ" + this.Var.toString() + "." + this.term.toString(); }
// }
// 
// class App implements Term { 
//   /*@ tag :: { number | v = 2 } */
//   tag: number = 2;
//   /*@ isValue :: { boolean | ~Prop(v) } */ 
//   isValue: boolean = false;
//   constructor(public t1: Term, public t2: Term) { }
//   
//   replace(v: Var, t: Term): Term {
//     return new App(this.t1.replace(v,t), this.t2.replace(v,t));
//   }
//   reduce(): Term {
//     if (!(this.t1.isValue)) { return new App(this.t1.reduce(), this.t2); }
//     else if (!(this.t2.isValue)) { return new App(this.t1, this.t2.reduce()); }
//     else if (this.t1 instanceof Lam) {
//     //else if (this.t1.tag === 1) /* Lam */ { 
//       var l1 = <Lam> this.t1;   //DOWNCAST
//       return l1.term.replace(l1.Var, this.t2);
//     }
//     alert("Reduction exception.");
//   }
//   toString(): string { return "(" + this.t1.toString() + ") (" + this.t2.toString() + ")" }
// }
// 
// 
// // TEST ////////////////////////////////////////////////////////////
// class Logger {
//   constructor() { this.messages = []; }
//   messages : string [];
//   write(s: string) { this.messages.push(s); }
//   dump() { alert(this.messages.join("\n")); }
// }
// 
// function run(program: Term) {
//   var logger = new Logger();
//   while(!program.isValue) {
//     logger.write(program.toString() + " -> ");
//     program = program.reduce();
//   }
//   logger.write(program.toString());
//   logger.dump();
// }
// 
// var t1 = new Lam(new Var("x"), new Var("x"));
// var t2 = new Lam(new Var("y"), new Var("y"));
// var t3 = new Lam(new Var("z"), t2);
// var t: Term = new App(t3,t2);
// run(t);
// 
// 
// var r1 = new Lam(new Var("z"), new Var("z"));
// var r2 = new Lam(new Var("y"), new Var("y"));
// var r3 = new App(new Var("x"), r2);
// var r4 = new Lam(new Var("x"), r3);
// var r: Term = new App(r4,r2);
// run(r);
// 
// var s1 = new Lam(new Var("z"), new Var("w"));
// var s2 = new Lam(new Var("y"), new Var("y"));
// var s3 = new App(new Var("w"), s2);
// var s4 = new Lam(new Var("x"), s3);
// var s: Term = new App(s4,s2);
// run(s);
// 
