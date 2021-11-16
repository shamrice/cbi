      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-11-16
      * Purpose: Assigns value to a variable
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. assign-var.

       environment division.
       
       configuration section.

       repository. 
           function ascii-code-to-char
           function inkey-func
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

      *> TODO: As of now, all numeric values are treated the same.       
       78  ws-suffix-type-string         value '$'.
       78  ws-suffix-type-int            value '%'.
       78  ws-suffix-type-long           value '&'.
       78  ws-suffix-type-single         value '!'.
       78  ws-suffix-type-double         value '#'.

       78  ws-add-operator               value '+'.
       78  ws-sub-operator               value '-'.
       78  ws-mult-operator              value '*'.
       78  ws-div-operator               value '/'.


       local-storage section.
       
       01  ls-var-idx                    pic 9(4) comp value zero.

       01  ls-var-source-idx             pic 9(4) comp.

       01  ls-space-count                pic 9(10) comp value zero.

       01  ls-end-quote-idx              pic 9(10) comp value zero.

       01  ls-assignment-dest            pic x(1024).

       01  ls-running-assign-val-type-sw pic a value 'N'.
           88  ls-assign-type-num        value 'N'.
           88  ls-assign-type-string     value 'S'.

       01  ls-running-assign-val         pic x(1024).
       01  ls-running-assign-val-num     pic 9(16).

       01  ls-temp-param-buffer          pic x(1024).
       01  ls-temp-param-value           pic x(1024).

       01  ls-temp-param-pointer         pic 9(4) comp.

       01  ls-temp-param-values          pic x(1024) occurs 10 times.        

       01  ls-temp-alloc-str             pic x(1024) value spaces.     

       01  ls-test-numval-ret-code       pic 99.   

       01  ls-allocate-return-code       pic 9 value 0.

       01  ls-suffix-counts.
           05  ls-numeric-suffix-count   pic 9(4).
           05  ls-string-suffix-count    pic 9(4).

       01  ls-is-first-value-sw          pic a value 'Y'.
           88  ls-is-first-value         value 'Y'.
           88  ls-is-not-first-value     value 'N'.

       01  ls-latest-operator            pic a value space.  

       01  ls-prev-operator            pic a value space.
           88  ls-prev-op-add          value '+'.
           88  ls-prev-op-sub          value '-'.
           88  ls-prev-op-mult         value '*'.
           88  ls-prev-op-div          value '/'.
           88  ls-prev-op-none         value space.            


       linkage section.       

       01  l-src-code-str                pic x(1024).       

       copy "copybooks/linkage_section/l_variable_table.cpy".
           

       procedure division using l-src-code-str l-variable-table.   

       main-procedure.

      *>     **** IN PROGRESS **** 
      *> TODO: Right hand side of assignment can be more than just a 
      *>       single value. Need to be able to handle stuff like:
      *>          x = y + 5 * z - 4


           unstring trim(l-src-code-str) 
               delimited by "="
               into ls-assignment-dest ls-temp-param-buffer
           end-unstring

      *>   TODO : currently will just treat CONST assignments as regular
      *>          assignments. Should be flagged so cannot be 
      *>          reassigned later.
           if upper-case(ls-assignment-dest(1:length(ws-const))) 
               = ws-const 
           then 
               move spaces 
               to ls-assignment-dest(1:length(ws-const))
           end-if 


      *> Find existing variable index if exists for assignment destination.
           perform varying ls-var-idx from 1 by 1 
           until ls-var-idx > l-num-variables

               if upper-case(ls-assignment-dest) 
               = l-variable-name(ls-var-idx) then 
                   if l-type-string(ls-var-idx) then 
                       set ls-assign-type-string to true 
                   else 
                       set ls-assign-type-num to true 
                   end-if 
                   exit perform 
               end-if 
           end-perform

      *> If not found, allocate a new variable before assignment.
           if l-num-variables = 0 or ls-var-idx > l-num-variables then 
               perform allocate-new-variable
           end-if 


           move 1 to ls-temp-param-pointer
           move trim(ls-temp-param-buffer) to ls-temp-param-buffer
           move spaces to ls-latest-operator           
           set ls-is-first-value to true 
           
           perform until 
               ls-temp-param-pointer > length(ls-temp-param-buffer)
               
               move ls-latest-operator to ls-prev-operator
               
      *>   TODO: This will currently split even if operator is in quoted
      *>         string!! that shouldn't be the case!!
               unstring ls-temp-param-buffer 
                   delimited by 
                       ws-add-operator 
                       or ws-sub-operator
                       or ws-mult-operator
                       or ws-div-operator                       
                   into ls-temp-param-value
                   delimiter in ls-latest-operator
                   with pointer ls-temp-param-pointer
               end-unstring

               call "logger" using "****************************"
               call "logger" using ls-temp-param-value 
               call "logger" using ls-latest-operator

               if ls-temp-param-value = spaces then 
                   exit perform 
               end-if 

               
      *> Check to see if right hand of assignment is variable. If so,
      *> substitute the correct value in its place.
               perform varying ls-var-source-idx from 1 by 1 
               until ls-var-source-idx > l-num-variables               

                   if trim(upper-case(ls-temp-param-value)) 
                       = l-variable-name(ls-var-source-idx) 
                   then 

                       call "logger" using concatenate(
                          "ASSIGNMENT :: Found righthand side variable:" 
                           trim(l-variable-name(ls-var-source-idx))
                           " value: " 
                           trim(l-variable-value(ls-var-source-idx))
                           " num val: " 
                           l-variable-value-num(ls-var-source-idx))
                       end-call 

                       if l-type-integer(ls-var-source-idx) then 
                           move l-variable-value-num
                               (ls-var-source-idx) 
                           to ls-temp-param-value 
                               
                       else 
                           move l-variable-value(ls-var-source-idx)
                           to ls-temp-param-value 
                               
                       end-if 
                       exit perform 
                   end-if 
               end-perform

              
               if ls-assign-type-string then 

                   move trim(ls-temp-param-value) to ls-temp-param-value

      *>           Check if value INKEY$
                   if upper-case(ls-temp-param-value) = ws-inkey then 
                       move function inkey-func
                       to ls-temp-param-value
                   end-if 

      *>           Check for CHR$
                   if upper-case(ls-temp-param-value(1:length(ws-chr)))
                       = ws-chr
                   then 
                       move ascii-code-to-char(
                           ls-temp-param-value, l-variable-table)
                           to ls-temp-param-value
                   end-if 

                   inspect ls-temp-param-value 
                       replacing all '"' by spaces 

                   if ls-is-first-value then 
                       move ls-temp-param-value 
                       to ls-running-assign-val
                       set ls-is-not-first-value to true 
                   else 
                       if ls-prev-op-add then 
                           string 
                               trim(ls-running-assign-val)
                               trim(ls-temp-param-value)
                               into ls-running-assign-val
                           end-string 
                       end-if 
                   end-if

               else 
                   if ls-is-first-value then 
                       move numval(ls-temp-param-value)
                       to ls-running-assign-val-num
                       set ls-is-not-first-value to true 
                   else 
                       evaluate true
                           when ls-prev-op-add
                               add numval(ls-temp-param-value)
                               to ls-running-assign-val-num

                           when ls-prev-op-sub
                               subtract numval(ls-temp-param-value)
                               from ls-running-assign-val-num

                           when ls-prev-op-mult                                   
                               multiply ls-running-assign-val-num
                               by numval(ls-temp-param-value) 
                               giving ls-running-assign-val-num

                           when ls-prev-op-div
                               divide ls-running-assign-val-num
                               by numval(ls-temp-param-value) 
                               giving ls-running-assign-val-num 
                         
                       end-evaluate                                   
                   end-if 
               end-if                                    
               
           end-perform


           
      *> Assign new value to variable      
           if l-type-integer(ls-var-idx) then 
               move trim(ls-running-assign-val-num)
                   to l-variable-value-num(ls-var-idx) 
               call "logger" using concatenate(
                   "ASSIGNMENT :: Number value. New value: "
                   l-variable-value-num(ls-var-idx) 
                   " : from: " trim(ls-running-assign-val-num))
               end-call                                              
           end-if 

           
           if l-type-string(ls-var-idx) then 
     
               move trim(ls-running-assign-val) 
               to ls-temp-param-value

               call "logger" using concatenate(
                   "ASSIGNMENT :: New raw assignment value: "
                   ls-running-assign-val)
               end-call 
      
               *>  remove leading and trailing '"' for strings
               *> TODO: DO NOT REMOVE QUOTES TO PRESERVE BLANK SPACES!
               inspect ls-temp-param-value
               replacing first '"' by space 

               move trim(ls-temp-param-value)
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
                           " assigned to type STRING but does "
                           "not have proper quotes in value. "
                           "Assigning anyway but data may be "
                           "incorrect.")
                       end-call 
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
               trim(ls-assignment-dest) 
               ". allocating new variable.")
           end-call            
  
      *> determine type by suffix. if none exists, assume integer,.
      *> set dest type switch as well

           inspect ls-assignment-dest tallying 
               ls-numeric-suffix-count for 
                   all ws-suffix-type-int,
                   all ws-suffix-type-long,
                   all ws-suffix-type-single,
                   all ws-suffix-type-double
               ls-string-suffix-count for 
                   all ws-suffix-type-string

           call "logger" using ls-numeric-suffix-count
           call "logger" using ls-string-suffix-count

      *> TODO: Later use tallies to determine exact data type.
           if ls-string-suffix-count > 0 then 
               set ls-assign-type-string to true 
               move concatenate(
                   ws-dim space 
                   trim(ls-assignment-dest) space
                   ws-string-type) 
                   to ls-temp-alloc-str               
           else 
               set ls-assign-type-num to true 
               move concatenate(
                   ws-dim space 
                   trim(ls-assignment-dest) space
                   ws-integer-type) 
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
                   trim(ls-assignment-dest))
               end-call 
               goback 
           end-if 

           exit paragraph.

       end program assign-var.
