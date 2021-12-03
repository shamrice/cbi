      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-12-02
      * Last Modified: 2021-12-03
      * Purpose: Process LEFT$: returns string after x num chars
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       function-id. left-func.

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

       
       01  ls-working-input-value     pic x(1024).

       01  ls-source-string           pic x(1024).
       01  ls-num-chars               pic 9(4) comp.


       linkage section.       

       01  l-input-value              pic x(1024).  

       01  l-output-value             pic x(1024).       


       procedure division 
           using l-input-value 
           returning l-output-value.
             
       main-procedure.

           call "logger" using concatenate(
               "LEFT-FUNC :: Enter with input: " trim(l-input-value))
           end-call 

           move spaces to l-output-value

           move trim(l-input-value) to ls-working-input-value

           move ws-left to ls-working-input-value(1:length(ws-left)) 
           
           inspect ls-working-input-value 
               replacing first ws-left by spaces 
               first "(" by space 

           move reverse(ls-working-input-value)
           to ls-working-input-value

           inspect ls-working-input-value
               replacing first ")" by space 

           move reverse(ls-working-input-value)
           to ls-working-input-value

           unstring ls-working-input-value
               delimited by "," 
               into ls-source-string, ls-num-chars
           end-unstring 


           if ls-source-string = spaces or ls-num-chars = 0 then 
               move l-input-value to l-output-value
               call "logger" using concatenate(
                   "LEFT-FUNC :: source string is empty or no number of"
                   " chars specified. Returning original string back.")
               end-call 
               goback 
           end-if 

           perform get-value-from-variable

           call "logger" using concatenate(
               "LEFT-FUNC :: source string=" trim(ls-source-string))
           end-call 

      *>   Add an extra one to account for the start quote in the str val
           add 1 to ls-num-chars

           string 
               ls-source-string(1:ls-num-chars) '"'
               into l-output-value
           end-string 
           
           call "logger" using concatenate(
               "LEFT-FUNC :: Returning:" trim(l-output-value))
           end-call 

           goback. 


       get-value-from-variable.
 
           move ls-source-string to ls-variable-name 
           call "get-variable" using
               ls-variable 
               ls-get-variable-return-code
           end-call 

           if ls-get-variable-return-code > 0 and ls-type-string then 
               move ls-variable-value to ls-source-string          
           end-if 

           exit paragraph.

       end function left-func.
