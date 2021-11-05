      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-26
      * Last Modified: 2021-11-05
      * Purpose: Searches variable table for variable name and sets the
      *          the return parameters to the found value. Return code 
      *          true(1) on success, false(0) on failure.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. get-var-value.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

         
       local-storage section.
 
       01  ls-var-idx                   pic 9(4) comp.       
   
       linkage section.       

       copy "copybooks/linkage_section/l_variable_table.cpy".       

       01  l-var-search-name             pic x(16).      

       01  l-return-type                 pic x(8).
           88  l-return-type-error       value spaces.

       01  l-return-val                  pic x(1024).  

       01  l-return-code                 pic 9 value 0.
           88  l-return-code-false       value 0.
           88  l-return-code-true        value 1.


       procedure division using 
           l-variable-table l-var-search-name 
           l-return-type l-return-val
           l-return-code.   

       main-procedure.

           set l-return-type-error to true 
           move spaces to l-return-val 
           set l-return-code-false to true 
           
           if l-num-variables = 0 or l-var-search-name = spaces then 
               call "logger" using concatenate(
                   "GET-VAR-VALUE :: WARNING : No variables or "
                   "variable name to get is blank. Num variables: " 
                   l-num-variables " : var-search-name: " 
                   l-var-search-name)
               end-call 
               goback 
           end-if 

           move upper-case(l-var-search-name) to l-var-search-name

           perform varying ls-var-idx from 1 by 1 
           until ls-var-idx > l-num-variables

               if l-variable-name(ls-var-idx) = l-var-search-name then 
                   move l-variable-type(ls-var-idx) 
                   to l-return-type
                   
                   if l-type-integer(ls-var-idx) then 
                       move l-variable-value-num(ls-var-idx) 
                       to l-return-val
                   else 
                       move l-variable-value(ls-var-idx) 
                       to l-return-val
                   end-if 

                   set l-return-code-true to true 
                   
                   exit perform 
               end-if 
           end-perform 

           call "logger" using concatenate(
               "GET-VAR-VALUE :: Variable name: " 
               upper-case(trim(l-var-search-name))
               " : type: " trim(l-return-type)
               " : value: " trim(l-return-val)
               " : return code: " l-return-code)
           end-call 

           goback.

       end program get-var-value.
