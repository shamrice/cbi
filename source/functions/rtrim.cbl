      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-12-02
      * Last Modified: 2021-12-02
      * Purpose: Process RTRIM$ - removes right blank spaces from string
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. rtrim is recursive.

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
       
       copy "copybooks/local_storage/ls_variable.cpy".
       
       01  ls-working-input-value    pic x(1024).

       linkage section.       

       01  l-input-value             pic x(1024).       


       procedure division 
           using l-input-value.
             
       main-procedure.

           move l-input-value to ls-working-input-value

      *>   Manually set to uppercase keyword... TODO: remove after
      *>   keywords are moved to uppercase during loading/parsing.
           move ws-rtrim to ls-working-input-value(1:length(ws-rtrim))

           call "logger" using concatenate(
               "RTRIM :: ENTER with: " trim(ls-working-input-value))
           end-call 

      *>   Replace RTRIM$ and it's parenthesis (but not any inside)
           inspect ls-working-input-value
               replacing 
                   all ws-rtrim by spaces 

           inspect ls-working-input-value
               replacing first "(" by space 

           move reverse(ls-working-input-value) 
           to ls-working-input-value

           inspect ls-working-input-value
               replacing first ")" by space 

           move reverse(ls-working-input-value)
           to ls-working-input-value

           call "logger" using concatenate(
               "RTRIM :: before var: " trim(ls-working-input-value))
           end-call 

           perform get-value-from-variable

           call "logger" using concatenate(
               "RTRIM :: after var: " trim(ls-working-input-value))
           end-call 

           move reverse(ls-working-input-value) 
           to ls-working-input-value

           inspect ls-working-input-value
               replacing first '"' by space 

           move reverse(ls-working-input-value)
           to ls-working-input-value

           move spaces to l-input-value

           string 
               trim(ls-working-input-value) '"'
               into l-input-value
           end-string 

           call "logger" using concatenate(
               "RTRIM :: final value: " l-input-value)
           end-call 

           goback.




       get-value-from-variable.
 
           move ls-working-input-value to ls-variable-name 
           call "get-variable" using
               ls-variable 
               ls-get-variable-return-code
           end-call 

           if ls-get-variable-return-code > 0 and ls-type-string then 
               move ls-variable-value to ls-working-input-value          
           end-if 

           exit paragraph.

       end program rtrim.
