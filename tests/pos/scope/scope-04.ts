
function f1() {
  var v1 = 1;

  function f2() {
    var v2 = 1;
    
    function f3() {
      var v3 = 1;

      function f4() {
        var v4 = 1;

        function f5() {
          /*@ v5 :: number */
          var v5 = 1;
          v5 ++;

          function f6() {
            var v6 = 1;
            v5 ++;

            function f7() {
              var v7 = 2;
  
            }
          }
        }
      }
    }
  }
}
