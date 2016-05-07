
/*@ global a_unique_obj :: { c: posint } */
let a_unique_obj = { c: 3 };

/*@ global another_obj :: { c: number } */
let another_obj = { c: 10 };

another_obj = a_unique_obj;
