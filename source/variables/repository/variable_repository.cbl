      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-18
      * Last Modified: 2021-12-02
      * Purpose: Holds all in-memory variable information.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. variable-repository is recursive.

       environment division.
       
       configuration section.

       repository. 
           function inkey-func
           function ascii-code-to-char
           function left-func 
           function right-func 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".       

       78  ws-type-int-const-val          value "INTEGER".
       78  ws-type-str-const-val          value "STRING".

       01  ws-var-end-idx                 usage index. 

       01  ws-variable-table.
           05  ws-num-variables           pic 9(4) comp.
           05  ws-variables               occurs 0 to 1000 times                                          
                                          depending on ws-num-variables
                                          indexed by ws-var-idx. 
               10  ws-variable-type       pic x(8) value spaces.
                   88  ws-type-integer    value ws-type-int-const-val.
                   88  ws-type-string     value ws-type-str-const-val.
               10  ws-variable-name       pic x(256) value spaces.
               10  ws-variable-value      pic x(1024) value spaces.
               10  ws-variable-value-num  pic S9(16) value zeros.  
    
       01  ws-return-code-name-sw         pic x(5) value "FALSE".
           88  ws-return-code-name-true   value "TRUE".
           88  ws-return-code-name-false  value "FALSE".


       local-storage section.       

       01  ls-found-var-idx               pic 9(4) comp.

       01  ls-var-save-action-sw          pic a(6) value "UPDATE".
           88  ls-var-save-action-new     value "NEW".
           88  ls-var-save-action-update  value "UPDATE".           

       01  ls-found-var-type              pic x(8).

       01  ls-temp-chr-check-string       pic x(1024).  
       01  ls-temp-inkey-ret-val          pic x(4).

       01  ls-temp-left-right-ret-val     pic x(1024).

       01  ls-leading-space-count         pic 9(4) comp.

       linkage section.       

       01  l-variable.               
           10  l-variable-type           pic x(8).
           10  l-variable-name           pic x(256).
           10  l-variable-value          pic x(1024).
           10  l-variable-value-num      pic S9(16).

       01  l-return-code                 pic 9 value 0.
           88  l-return-code-false       value 0.
           88  l-return-code-true        value 1.


       procedure division using 
           l-variable l-return-code. 

       main-procedure.
           call "logger" using concatenate(
               "VARIABLE-REPOSITORY:: ERROR : Variable repository "
               "should not be called directly. Use 'set-variable' or "
               "'get-variable' entry points instead.")
           end-call
           goback. 



      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-18
      * Last Modified: 2021-11-18
      * Purpose: Entry point to set variable value. If existing variable
      *          is found, it will be updated. If no variable is found,
      *          a new one will be allocated.
      * Tectonics: ./build.sh
      ******************************************************************
       entry "set-variable" using l-variable.

           move upper-case(trim(l-variable-name)) to l-variable-name 

           if ws-num-variables > 0 then 
               set ws-var-end-idx to ws-num-variables
               perform varying ws-var-idx from 1 by 1 
               until ws-var-idx > ws-var-end-idx
               
                   if ws-variable-name(ws-var-idx) = l-variable-name 
                   then 
                       move ws-var-idx to ls-found-var-idx                       
                       exit perform 
                   end-if 
               end-perform 
           end-if 
           
           if ls-found-var-idx = 0 then 
               add 1 to ws-num-variables
               move ws-num-variables to ls-found-var-idx
               set ls-var-save-action-new to true 
           else 
               move ws-variable-type(ls-found-var-idx) 
               to l-variable-type 
           end-if 

           move l-variable to ws-variables(ls-found-var-idx)                      

           call "logger" using concatenate(
               "VARIABLE-REPOSITORY::SET-VARIABLE"
               " : Action: " ls-var-save-action-sw
               " : name: " trim(ws-variable-name(ls-found-var-idx))
               " : type: " ws-variable-type(ls-found-var-idx) 
               " : num value: " ws-variable-value-num(ls-found-var-idx)
               " : value: " trim(ws-variable-value(ls-found-var-idx)))             
           end-call 

           goback.



      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-18
      * Last Modified: 2021-12-03
      * Purpose: Entry point to get variable based on name passed in the
      *          l-variable record. Return code is false if no existing 
      *          record is found or true if one is found and the l-variable
      *          record is populated.
      * Tectonics: ./build.sh
      ******************************************************************
       entry "get-variable" using l-variable l-return-code.

           set l-return-code-false to true 
           move zeros to l-variable-value-num
           move spaces to l-variable-value           
           move spaces to l-variable-type

           if l-variable-name = spaces then 
               goback 
           end-if 

      *>   If there's any leading spaces, shift them out of the string.
           inspect l-variable-name 
               tallying ls-leading-space-count
               for leading spaces                      

           if ls-leading-space-count > 0 then 
               add 1 to ls-leading-space-count
               move l-variable-name(ls-leading-space-count:) 
               to l-variable-name 
           end-if 


      *>   Check if val should be subbed with INKEY$ value.
           if  upper-case(l-variable-name) = ws-inkey then 
               move function inkey-func to l-variable-value      

               set ws-return-code-name-true to true 
               set l-return-code-true to true 
               move ws-type-str-const-val to l-variable-type

               perform log-get-variable
               goback             
               
           end-if 

      *>   Check for CHR$() function.
           if upper-case(l-variable-name(1:length(ws-chr))) = ws-chr 
           then                
               move ascii-code-to-char(l-variable-name)
               to ls-temp-chr-check-string
              
               string 
                   '"' 
                   trim(ls-temp-chr-check-string)
                   '"'
                   into l-variable-value
               end-string 

               set ws-return-code-name-true to true 
               set l-return-code-true to true 
               move ws-type-str-const-val to l-variable-type  

               perform log-get-variable
               goback 
           end-if  

      *>   Check for RTRIM$() function.
           if upper-case(l-variable-name(1:length(ws-rtrim))) = ws-rtrim
           then      
               move l-variable-name to l-variable-value          
               call "rtrim" using l-variable-value 
              
               set ws-return-code-name-true to true 
               set l-return-code-true to true 
               move ws-type-str-const-val to l-variable-type  

               perform log-get-variable
               goback 
           end-if 

      *>   Check for LTRIM$() function.
           if upper-case(l-variable-name(1:length(ws-ltrim))) = ws-ltrim
           then      
               move l-variable-name to l-variable-value          
               call "ltrim" using l-variable-value 
              
               set ws-return-code-name-true to true 
               set l-return-code-true to true 
               move ws-type-str-const-val to l-variable-type  

               perform log-get-variable
               goback 
           end-if 

           
      *>   Check for LEFT$() function.
           if upper-case(l-variable-name(1:length(ws-left))) = ws-left
           then     
               *> For some reason, it needs to the temp var to jump from
               move left-func(l-variable-name) 
               to ls-temp-left-right-ret-val               
               
               move ls-temp-left-right-ret-val to l-variable-value

               set ws-return-code-name-true to true 
               set l-return-code-true to true 
               move ws-type-str-const-val to l-variable-type  

               perform log-get-variable
               goback 
           end-if 

      *>   Check for RIGHT$() function.
           if upper-case(l-variable-name(1:length(ws-right))) = ws-right
           then     
               *> For some reason, it needs to the temp var to jump from
               move right-func(l-variable-name) 
               to ls-temp-left-right-ret-val               
               
               move ls-temp-left-right-ret-val to l-variable-value

               set ws-return-code-name-true to true 
               set l-return-code-true to true 
               move ws-type-str-const-val to l-variable-type  

               perform log-get-variable
               goback 
           end-if        


           if ws-num-variables = 0 or l-variable-name = spaces then 
               goback 
           end-if 

           move upper-case(trim(l-variable-name)) to l-variable-name 

           call "array-indexed-name" using l-variable-name 

           set ws-var-end-idx to ws-num-variables
           perform varying ws-var-idx from 1 by 1 
           until ws-var-idx > ws-var-end-idx
               
               if ws-variable-name(ws-var-idx) = l-variable-name 
               then 
                   move ws-variables(ws-var-idx) to l-variable 
                   set l-return-code-true to true 
                   exit perform 
               end-if 
           end-perform 
                      
           if l-return-code-true then 
               set ws-return-code-name-true to true 
           else 
               set ws-return-code-name-false to true 
           end-if 

           perform log-get-variable

           goback.



       log-get-variable.

         call "logger" using concatenate(
               "VARIABLE-REPOSITORY::GET-VARIABLE" 
               " : Return code: " l-return-code  
               " (" ws-return-code-name-sw ")"            
               " : name: " trim(l-variable-name)
               " : type: " l-variable-type 
               " : num value: " l-variable-value-num
               " : value: " trim(l-variable-value))             
           end-call 

           exit paragraph.

       end program variable-repository.
