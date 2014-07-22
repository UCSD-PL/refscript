var glob /*@ { number | v > 10 } */ = 20;

/*@ zog :: () => {void | true} */
function zog(){
  glob = 3;
}
