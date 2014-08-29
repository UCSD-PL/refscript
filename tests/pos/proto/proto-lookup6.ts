function foo(o, s)
/*:
    (rec o . { __proto__: Object, *: Num + 'o, /___(.*)_/: Any})
  * /([^_])(.*)([^_])/
 -> Undef + Num + (rec o . { __proto__: Object, *: Num + 'o, /___(.*)_/: Any})
*/
{
    return o[s];
}
