      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-10-12
      * Purpose: Allocates a new variable
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. allocate-var.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       01  ws-temp-cmd-buffer            pic x(256).
       01  ws-temp-param-buffer          pic x(1024).

       local-storage section.
       01  ls-keyword-count              pic 9(10) comp value zero.
       01  ls-assignment-count           pic 9 comp value zero.

       01  ls-temp-param-values          pic x(1024) occurs 10 times.

       01  ls-temp-variable-type         pic x(10) value spaces.

       01  ls-temp-variable-idx          pic 9(4) comp value 0.

       01  ls-temp-cmd-buffer            pic x(256) value spaces.
       01  ls-temp-param-buffer          pic x(1024) value zeros.

       01  ls-is-valid-variable-sw       pic a value 'N'.
           88  ls-is-valid-variable      value 'Y'.
           88  ls-not-valid-variable     value 'N'.

       
       linkage section.       

       01  l-src-code-str                pic x(1024). 

       01  l-variable-table.
           05  l-num-variables           pic 9(4) comp.
           05  l-variables               occurs 0 to unbounded times
                                         depending on l-num-variables. 
               10  l-variable-type       pic x(8) value spaces.
                   88  l-type-integer    value "INTEGER".
                   88  l-type-string     value "STRING".
               10  l-variable-name       pic x(16) value spaces.
               10  l-variable-value      pic x(1024) value spaces.
               10  l-variable-value-num  redefines l-variable-value
                                         pic 9(16) value zeros.       

       procedure division using l-src-code-str l-variable-table.       
      
       main-procedure.

           call "logger" using concatenate(
               "DIM :: attempt to parse source line: " 
               trim(l-src-code-str))
           end-call 

           move upper-case(trim(l-src-code-str(4:)))
               to ls-temp-param-buffer

           inspect ls-temp-param-buffer  
               tallying ls-keyword-count for all ws-string-type 
                   
           if ls-keyword-count = 0 then 
               call "logger" using "New var not STRING, checking INT"
               
               inspect ls-temp-param-buffer
               tallying ls-keyword-count for all ws-integer-type

               if ls-keyword-count = 0 then 
                   call "logger" using "Cannot determine type. Skipping"
                   exit paragraph
               else 
                   move ws-integer-type to ls-temp-variable-type
               end-if 
           else 
               move ws-string-type to ls-temp-variable-type
           end-if 

           add 1 to l-num-variables

           move spaces to l-variable-value(l-num-variables)

           if ls-temp-variable-type = ws-string-type
               set l-type-string(l-num-variables) to true 
           else 
               set l-type-integer(l-num-variables) to true 
           end-if 

           unstring ls-temp-param-buffer
               delimited by space 
               into ls-temp-param-values(1) 
           end-unstring

           move ls-temp-param-values(1) 
               to l-variable-name(l-num-variables)

           call "logger" using concatenate(
               "DIM :: name: " trim(l-variable-name(l-num-variables))
               " value: " trim(l-variable-value(l-num-variables))
               " type: " trim(l-variable-type(l-num-variables)))
           end-call 
       
           goback.

       end program allocate-var.

