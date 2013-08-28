var array = [4, 5, 2, 6, 7];

function insertionSort(sortMe) {
   for (var i = 0, j, tmp; i < sortMe.length; ++i) {
      tmp = sortMe[i];
      for (j = i - 1; j >= 0 && sortMe[j] > tmp; --j)
         sortMe[j + 1] = sortMe[j];
      sortMe[j + 1] = tmp;
   }
}

insertionSort(array)
array[1]
