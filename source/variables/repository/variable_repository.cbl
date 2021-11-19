      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-18
      * Last Modified: 2021-11-18
      * Purpose: Holds all in-memory variable information.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. variable-repository.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       01  ws-var-end-idx                 usage index. 

       01  ws-variable-table.
           05  ws-num-variables           pic 9(4) comp.
           05  ws-variables               occurs 0 to 1000 times                                          
                                          depending on ws-num-variables
                                          indexed by ws-var-idx. 
               10  ws-variable-type       pic x(8) value spaces.
                   88  ws-type-integer    value "INTEGER".
                   88  ws-type-string     value "STRING".
               10  ws-variable-name       pic x(16) value spaces.
               10  ws-variable-value      pic x(1024) value spaces.
               10  ws-variable-value-num  pic S9(16) value zeros.  
    
       local-storage section.       

       01  ls-found-var-idx               pic 9(4) comp.

       01  ls-var-save-action-sw          pic a(6) value "UPDATE".
           88  ls-var-save-action-new     value "NEW".
           88  ls-var-save-action-update  value "UPDATE".

       01  ls-found-var-type              pic x(8).

       linkage section.       

       01  l-variable.               
           10  l-variable-type       pic x(8).
           10  l-variable-name       pic x(16).
           10  l-variable-value      pic x(1024).
           10  l-variable-value-num  pic S9(16).

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
      * Last Modified: 2021-11-18
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

           if ws-num-variables = 0 or l-variable-name = spaces then 
               goback 
           end-if 

           move upper-case(trim(l-variable-name)) to l-variable-name 

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
                      
           call "logger" using concatenate(
               "VARIABLE-REPOSITORY::GET-VARIABLE" 
               " : Return code: " l-return-code              
               " : name: " trim(l-variable-name)
               " : type: " l-variable-type 
               " : num value: " l-variable-value-num
               " : value: " trim(l-variable-value))             
           end-call 

           goback.
       end program variable-repository.
