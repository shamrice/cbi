      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-05
      * Last Modified: 2021-11-18
      * Purpose: Copybook containing definitions for if boundry table
      * Tectonics: ./build.sh
      ******************************************************************
       01  l-if-boundary-table.
           05  l-num-ifs                  pic 9(4) comp. 
           05  l-if-data                  occurs 0 to unbounded times
                                          depending on l-num-ifs.
               10  l-if-processed-sw      pic a.
                   88  l-if-processed     value 'Y'.
                   88  l-if-not-processed value 'N'.
               10  l-if-start             pic 9(5).
               10  l-num-elseifs          pic 99 comp.
               10  l-elseif-start         pic 9(5) occurs 99 times.
               10  l-else-start           pic 9(5). 
               10  l-if-end               pic 9(5).
