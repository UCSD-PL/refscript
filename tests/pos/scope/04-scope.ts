
function f1() {
    let v1 = 1;

    function f2() {
        let v2 = 1;

        function f3() {
            let v3 = 1;

            function f4() {
                let v4 = 1;

                function f5() {
                    /*@ v5 :: number */
                    let v5 = 1;
                    v5++;

                    function f6() {
                        let v6 = 1;
                        v5++;

                        function f7() {
                            let v7 = 2;

                        }
                    }
                }
            }
        }
    }
}
