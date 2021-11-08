      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-08
      * Last Modified: 2021-11-08
      * Purpose: Process INKEY$ - get kb input.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       function-id. inkey-func.

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
       

       linkage section.       

       01  l-user-input              pic x(1). 

       procedure division returning l-user-input.

           set environment "COB_TIMEOUT_SCALE" to '3'.
              
       main-procedure.
           
      *>   TODO : use crtstatus to get values of arrow keys and such.

           move spaces to l-user-input

      *>   Hide cursor... in fast loops the hide/unhide still flashes.
      *     call static "curs_set" using by value 0 
          
           accept l-user-input                
               with 
               auto-skip no-echo 
               timeout after 5 
           end-accept            
       
      *     call static "curs_set" using by value 1

           goback.
           

       end function inkey-func.
