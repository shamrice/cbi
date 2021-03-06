      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-11-19
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

       copy "copybooks/local_storage/ls_variable.cpy".       

       01  ls-keyword-count              pic 9(10) comp value zero.       

       01  ls-temp-param-values          pic x(1024).

       01  ls-temp-variable-type         pic x(10) value spaces.       
       
       01  ls-temp-param-buffer          pic x(1024) value zeros.

       01  ls-keyword-check-ret-code     pic 9 value 0.

       linkage section.       

       01  l-src-code-str                pic x(1024). 

       01  l-allocate-return-code        pic 9 value 0.
           88  l-return-code-true        value 1.
           88  l-return-code-false       value 0.      

       procedure division using 
           l-src-code-str l-allocate-return-code.       
      
       main-procedure.

           call "logger" using concatenate(
               "DIM :: attempt to parse source line: " 
               trim(l-src-code-str))
           end-call 

      *> TODO : currently dim & dim shared are treated the same. 
           if upper-case(l-src-code-str(1:length(ws-dim-shared))) 
               = ws-dim-shared
           then 
               move upper-case(trim(
                   l-src-code-str(length(ws-dim-shared):)))
                   to ls-temp-param-buffer
           else                 
               move upper-case(trim(l-src-code-str(length(ws-dim):)))
                   to ls-temp-param-buffer
           end-if 

      *>   Get and set variable name as well as increment variable count.
           unstring ls-temp-param-buffer
               delimited by space 
               into ls-temp-param-values 
           end-unstring

      *>   Make sure var name isn't a reserve word. If so, exit allocation.
           call "is-keyword" using 
               ls-temp-param-values
               ls-keyword-check-ret-code
           end-call                    

           if ls-keyword-check-ret-code = 1 then 
               call "logger" using concatenate(
                   "DIM :: cannot allocate variable. Variable name is "
                   " a reserved keyword. Variable name attempted: "
                   trim(ls-temp-param-values))
               end-call
               set l-return-code-false to true 
               goback 
           end-if 
              
           move ls-temp-param-values
               to ls-variable-name 

      *>   Figure out what the new type is for the variable and set it.
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

      *>   Allocate variable with blank value.
           move spaces to ls-variable-value

           if ls-temp-variable-type = ws-string-type      
               set ls-type-string to true 
           else       
               set ls-type-integer to true 
           end-if 

           call "set-variable" using ls-variable 

           call "logger" using concatenate(
               "DIM :: name: " trim(ls-variable-name)
               " value: " trim(ls-variable-value)
               " type: " trim(ls-variable-type))
           end-call 
      
       
           set l-return-code-true to true 
           goback.

       end program allocate-var.

