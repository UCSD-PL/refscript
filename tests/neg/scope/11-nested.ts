//
// /*  foo :: () => { number | v = 5 } */
// export function foo() {
//     let u = 1;
//     /* addThree :: (u: number) => { number | v = u + 3 } */
//     function addThree(u: number) {
//         return u + 1;
//     }
//     return addThree(u);
// }
//
// foo();
//


/*@  foo :: () => { number | v = 5 } */
export function foo() {

    // { k__ 2 } => { k__4 }
    function addThree(xxxx: number) {
        return xxxx + 1;
    }

    return addThree(1);
}



// 1.                   { k__4  }     <: { v = 5 }
// 2.                   { v = 1 }     <: { k__2  }
// 3.    x: { k__2 } |- { v = x + 1 } <: { k__4 }
