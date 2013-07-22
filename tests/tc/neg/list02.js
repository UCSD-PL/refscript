/*@ type nlist  { data : number, 
                  next : nlist } */


/*@ main :: (x:nlist, a:number|boolean) => nlist */
function main(x, a) {

    return { data: a , next: x };

}


