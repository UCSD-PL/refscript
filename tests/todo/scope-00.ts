
/*@ myassert :: forall A . ({x:A|(Prop x)}) => void */
declare function myassert<A>(x: A): void;

/*@ myrandom :: () => {v:number | true} */
declare function myrandom(): number;


/*@ qualif CondLock1(v:number,x:number): v = (if (0 <= x) then 1 else 0)  */    


myassert(a.f === 1);
myassert(b === 1);


var a = { f: 1 };

var b = 1;
