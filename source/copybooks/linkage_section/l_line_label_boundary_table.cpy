      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-08
      * Last Modified: 2021-11-19
      * Purpose: Copybook containing definitions for line label boundry 
      *          table
      * Tectonics: ./build.sh
      ******************************************************************
       01  l-line-label-boundary-table.
           05  l-num-line-labels         pic 9(4) comp. 
           05  l-line-label-data         occurs 0 to unbounded times
                                         depending 
                                         on l-num-line-labels
                                         indexed by l-label-idx.    
               10  l-label-name          pic x(32).           
               10  l-label-start         pic 9(5). *>TODO Make comp 
               10  l-label-end           pic 9(5). *> For GOSUBs RETURN
               10  l-label-last-call     pic 9(5). *> No nesting in GOSUB
