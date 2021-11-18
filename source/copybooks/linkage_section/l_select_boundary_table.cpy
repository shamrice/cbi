      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-13
      * Last Modified: 2021-11-18
      * Purpose: Copybook containing definitions for SELECT CASE
      *          boundry table
      * Tectonics: ./build.sh
      ******************************************************************
       01  l-select-boundary-table.
           05  l-num-selects                  pic 9(4) comp. 
           05  l-select-data                  occurs 0 to unbounded 
                                              times depending on 
                                              l-num-selects.
               10  l-select-processed-sw      pic a.
                   88  l-select-processed     value 'Y'.
                   88  l-select-not-processed value 'N'.                   
               10  l-select-start             pic 9(5).
               10  l-select-check-val         pic x(1024).
               10  l-num-cases                pic 99 comp.
               10  l-case-start               pic 9(5)  
                                              occurs 99 times.
               10  l-select-end               pic 9(5).
