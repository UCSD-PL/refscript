
/*@ foo :: ([Immutable]{ f: ({ string | v = "aaa"})=>string })=>void */
function foo(f: { f: any }) {

}

/*@ x :: [Immutable]
         {  f: /\ ({ string | v = "aaa"})=>string
               /\ ({ number | v > 0    })=>number 
         } */
declare var x;

foo(x);

