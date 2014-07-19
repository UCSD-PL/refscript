
/*@ foo :: ([#Immutable]{ f: ({ string | v = "aaa"})=>string })=>void */
function foo(f: { f: any }) {

}

/*@ x :: [#Immutable]
         {  f: /\ ({ string | v = "aa"})=>string
               /\ ({ number | v > 0    })=>number 
         } */
var x;

foo(x);

