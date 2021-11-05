      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-05
      * Last Modified: 2021-11-05
      * Purpose: Copybook containing definitions for loop boundry table
      * Tectonics: ./build.sh
      ******************************************************************
       01  l-loop-boundary-table.
           05  l-num-loops           pic 9(4) comp value 0. 
           05  l-loop-data           occurs 0 to unbounded times
                                     depending on l-num-loops.               
               10  l-loop-start      pic 9(5). *>TODO Make comp 
               10  l-loop-end        pic 9(5).
               