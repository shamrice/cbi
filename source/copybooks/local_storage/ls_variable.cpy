      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-18
      * Last Modified: 2021-11-18
      * Purpose: Copybook containing definitions for variable
      * Tectonics: ./build.sh
      ******************************************************************
       01  ls-variable.                      
           05  ls-variable-type       pic x(8) value spaces.
               88  ls-type-integer    value "INTEGER".
               88  ls-type-string     value "STRING".
           05  ls-variable-name       pic x(16) value spaces.
           05  ls-variable-value      pic x(1024) value spaces.
           05  ls-variable-value-num  pic 9(16) value zeros.   

       01  ls-get-variable-return-code pic 9.
       