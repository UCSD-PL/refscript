

/*@ getHead :: (list[number] + null) => {number | true} */
function getHead(xs){
  if (empty(xs)) {
    return 1;
  }
  return safehead(xs);
}


