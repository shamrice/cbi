      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-14
      * Last Modified: 2021-10-14
      * Purpose: Process the SLEEP command with parameter.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. sleep-program.

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

       01  ls-sleep-duration            pic 9(8) value zero.

       linkage section.       

       01  l-src-code-str               pic x(1024). 
      
       procedure division using l-src-code-str.   

       main-procedure.

           move l-src-code-str(length(ws-sleep) + 1:)
               to ls-temp-param-buffer
           
           if ls-temp-param-buffer not = spaces 
           and test-numval(ls-temp-param-buffer) = 0 then 
               
               move ls-temp-param-buffer to ls-sleep-duration
               
               accept omitted
                   with no-echo time-out after ls-sleep-duration
               end-accept 
           else 

               accept omitted with no-echo 
           end-if               

           call "logger" using concatenate(
               "SLEEP :: Sleeping program. Duration? " 
               ls-sleep-duration)
           end-call 
              
           goback.

       end program sleep-program.
