      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-05
      * Last Modified: 2021-11-19
      * Purpose: Copybook containing definitions for sub boundry table
      * Tectonics: ./build.sh
      ******************************************************************
       01  l-sub-boundary-table.
           05  l-num-subs            pic 9(4) comp. 
           05  l-sub-data            occurs 0 to 1000 times
                                     depending on l-num-subs
                                     indexed by l-sub-idx.  
               10  l-sub-name        pic x(32).
               10  l-sub-start       pic 9(5). *>TODO Make comp 
               10  l-sub-end         pic 9(5).  
               10  l-sub-cur-nest    pic 9(4) value 0.
               10  l-sub-last-call   pic 9(5) occurs 1000 times.
                                     *>idx of last call is cur nest. 
