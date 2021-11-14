      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-11-14
      * Purpose: Assigns value to a variable
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. assign-var.

       environment division.
       
       configuration section.

       repository. 
           function inkey-func
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       local-storage section.
       
       01  ls-var-idx                    pic 9(4) comp value zero.

       01  ls-var-source-idx             pic 9(4) comp.

       01  ls-space-count                pic 9(10) comp value zero.

       01  ls-end-quote-idx              pic 9(10) comp value zero.

       01  ls-temp-param-values          pic x(1024) occurs 10 times.  

       01  ls-temp-alloc-str             pic x(1024) value spaces.     

       01  ls-test-numval-ret-code       pic 99.   

       01  ls-allocate-return-code        pic 9 value 0.

       linkage section.       

       01  l-src-code-str                pic x(1024).       

       copy "copybooks/linkage_section/l_variable_table.cpy".
           

       procedure division using l-src-code-str l-variable-table.   

       main-procedure.

      *> TODO: Right hand side of assignment can be more than just a 
      *>       single value. Need to be able to handle stuff like:
      *>          x = y + 5 * z - 4


           unstring trim(l-src-code-str) 
               delimited by "="
               into ls-temp-param-values(1) ls-temp-param-values(2) 
           end-unstring

      *>   TODO : currently will just treat CONST assignments as regular
      *>          assignments. Should be flagged so cannot be 
      *>          reassigned later.
           if upper-case(ls-temp-param-values(1)(1:length(ws-const))) 
               = ws-const 
           then 
               move spaces 
               to ls-temp-param-values(1)(1:length(ws-const))
           end-if 

      *> Check to see if right hand of assignment is variable. If so,
      *> substitute the correct value in its place.
           perform varying ls-var-source-idx from 1 by 1 
           until ls-var-source-idx > l-num-variables               

               if trim(upper-case(ls-temp-param-values(2))) 
                   = l-variable-name(ls-var-source-idx) 
               then 

                   call "logger" using concatenate(
                       "ASSIGNMENT :: Found righthand side variable: " 
                       trim(l-variable-name(ls-var-source-idx))
                       " value: " 
                       trim(l-variable-value(ls-var-source-idx))
                       " num val: " 
                       l-variable-value-num(ls-var-source-idx))
                   end-call 

                   if l-type-integer(ls-var-source-idx) then 
                       move l-variable-value-num(ls-var-source-idx) 
                       to ls-temp-param-values(2) 
                   else 
                       move l-variable-value(ls-var-source-idx)
                       to ls-temp-param-values(2) 
                   end-if 
                   exit perform 
               end-if 
           end-perform


      *> Find existing variable index if exists for assignment destination.
           perform varying ls-var-idx from 1 by 1 
           until ls-var-idx > l-num-variables

               if upper-case(ls-temp-param-values(1)) 
               = l-variable-name(ls-var-idx) then 
                   exit perform 
               end-if 
           end-perform

      *> If not found, allocate a new variable before assignment.
           if l-num-variables = 0 or ls-var-idx > l-num-variables then 
               perform allocate-new-variable
           end-if 

           
      *> Assign new value to variable      
           if l-type-integer(ls-var-idx) then 
               move trim(ls-temp-param-values(2))
                   to l-variable-value-num(ls-var-idx) 
               call "logger" using concatenate(
                   "ASSIGNMENT :: Number value. New value: "
                   l-variable-value-num(ls-var-idx) 
                   " : from: " trim(ls-temp-param-values(2)))
               end-call                                              
           end-if 

           
           if l-type-string(ls-var-idx) then 
     
      *>         Check if value INKEY$
               if trim(ls-temp-param-values(2)) = ws-inkey then 
                   move function inkey-func
                   to l-variable-value(ls-var-idx)    
               
               else 
               *> remove leading and trailing '"' for strings
                   inspect ls-temp-param-values(2)
                   replacing first '"' by space 

                   move trim(ls-temp-param-values(2))
                       to l-variable-value(ls-var-idx)
               
                   move zeros to ls-space-count   
                       
                   inspect reverse(l-variable-value(ls-var-idx))
                   tallying ls-space-count for leading spaces
               
                   compute ls-end-quote-idx = 
                       length(l-variable-value(ls-var-idx)) 
                       - ls-space-count
                   end-compute 
               
                   if ls-end-quote-idx > 0 then 
                       if l-variable-value
                           (ls-var-idx)(ls-end-quote-idx:1) = '"' 
                       then                
                           move spaces 
                           to l-variable-value
                               (ls-var-idx)(ls-end-quote-idx:)
                       else 
                           call "logger" using concatenate(
                               "ASSIGNMENT :: WARNING : variable: " 
                               trim(l-variable-name(ls-var-idx))
                               " assigned to type STRING but does not "
                               "have proper quotes in value. Assigning"
                               " anyway but data may be incorrect.")
                           end-call 
                       end-if 
                   end-if 
               end-if 
           end-if 

           call "logger" using concatenate(
               "ASSIGNMENT :: variable name: " 
               trim(l-variable-name(ls-var-idx))
               " new value: " trim(l-variable-value(ls-var-idx))
               " type: " l-variable-type(ls-var-idx)
               " space count: " ls-space-count)
           end-call                     

           goback. 


       allocate-new-variable.
           call "logger" using concatenate(
               "ASSIGNMENT :: No variables exist yet for "
               trim(ls-temp-param-values(1)) 
               ". allocating new variable.")
           end-call            

      *> Determine if value is an integer or not and build dimension
      *> allocation string     
           move test-numval(ls-temp-param-values(2)) 
               to ls-test-numval-ret-code

           if ls-test-numval-ret-code = 0 then 
               move concatenate(
                   ws-dim space 
                   trim(ls-temp-param-values(1)) space
                   ws-integer-type) 
                   to ls-temp-alloc-str
           else 
               move concatenate(
                      ws-dim space 
                   trim(ls-temp-param-values(1)) space
                   ws-string-type) 
                   to ls-temp-alloc-str
           end-if

           call "allocate-var" using 
               ls-temp-alloc-str
               l-variable-table
               ls-allocate-return-code
           end-call        

           if ls-allocate-return-code = 0 then 
               call "logger" using concatenate(
                   "ASSIGNMENT :: cannot assign value. Allocation "
                   "of new variable failed. Variable: " 
                   trim(ls-temp-param-values(1)))
               end-call 
               goback 
           end-if 

           exit paragraph.

       end program assign-var.
