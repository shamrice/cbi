      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-10-12
      * Purpose: Processed the LOCATE command and sets cursor position
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. set-cursor-position.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       local-storage section.
       
       01  ls-temp-param-buffer         pic x(1024).
       01  ls-temp-param-values         pic x(1024) occurs 10 times.          

       linkage section.       

       01  l-src-code-str               pic x(1024). 

       01  l-screen-position.
           05  l-scr-row                pic 999.
           05  l-scr-col                pic 999.    


       procedure division using l-src-code-str l-screen-position.   

       main-procedure.

           move trim(l-src-code-str(7:)) to ls-temp-param-buffer
                         
           unstring ls-temp-param-buffer
               delimited by "," 
               into 
                   ls-temp-param-values(1) 
                   ls-temp-param-values(2)
           end-unstring

           move ls-temp-param-values(1) to l-scr-row
           move ls-temp-param-values(2) to l-scr-col

           call "logger" using concatenate(
               "LOCATE :: row: " l-scr-row
               " col: " l-scr-col)
           end-call 
              
           goback.

       end program set-cursor-position.
