      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-08
      * Last Modified: 2021-11-15
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
       
       01  ls-user-key-in            pic x.

       linkage section.       

       01  l-user-input              pic xx. 

       procedure division returning l-user-input.

           set environment "COB_TIMEOUT_SCALE" to '3'.
              
       main-procedure.
           
      *>   TODO : use crtstatus to get values of arrow keys and such.

           move spaces to l-user-input

      *>   Hide cursor until it's unhidden again.
           call static "curs_set" using by value 0 
          
           accept ls-user-key-in    
               at 0181            
               with 
               auto-skip no-echo 
               timeout after 5 
           end-accept                             

      *> Do additional transformation here for arrow keys and function 
      *> keys
           move ls-user-key-in to l-user-input

           goback.
           

       end function inkey-func.
