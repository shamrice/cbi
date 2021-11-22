      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-11-22
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
       
       copy "copybooks/local_storage/ls_variable.cpy".  

       01  ls-temp-variable.                      
           05  ls-temp-variable-type       pic x(8) value spaces.
               88  ls-temp-type-integer    value "INTEGER".
               88  ls-temp-type-string     value "STRING".
           05  ls-temp-variable-name       pic x(16) value spaces.
           05  ls-temp-variable-value      pic x(1024) value spaces.
           05  ls-temp-variable-value-num  pic S9(16) value zeros.                 

       01  ls-space-count                pic 9(10) comp value zero.

       01  ls-end-quote-idx              pic 9(10) comp value zero.

       01  ls-char-idx                  pic 9(4) comp. 
       01  ls-trailing-quote-idx        pic 9(4) comp.

       01  ls-assignment-dest            pic x(1024).

       01  ls-running-assign-val-type-sw pic a value 'N'.
           88  ls-assign-type-num        value 'N'.
           88  ls-assign-type-string     value 'S'.

       01  ls-running-assign-val         pic x(1024).
       01  ls-running-assign-val-num     pic S9(16).

       01  ls-running-assign-val-num-disp pic x(17).
       01  ls-variable-value-num-disp     pic x(17).

       01  ls-temp-param-buffer          pic x(1024).
       01  ls-temp-param-value           pic x(1024).     
       01  ls-temp-chr-check-string      pic x(1024).  
       01  ls-temp-inkey-ret-val         pic xx.

       01  ls-temp-param-pointer         pic 9(4) comp.
       

       01  ls-temp-alloc-str             pic x(1024) value spaces.            

       01  ls-allocate-return-code       pic 9 value 0.

       01  ls-suffix-counts.
           05  ls-numeric-suffix-count   pic 9(4) comp.
           05  ls-string-suffix-count    pic 9(4) comp.

       01  ls-is-first-value-sw          pic a value 'Y'.
           88  ls-is-first-value         value 'Y'.
           88  ls-is-not-first-value     value 'N'.

       01  ls-latest-operator            pic a value space.  

       01  ls-prev-operator              pic a value space.
           88  ls-prev-op-add            value '+'.
           88  ls-prev-op-sub            value '-'.
           88  ls-prev-op-mult           value '*'.
           88  ls-prev-op-div            value '/'.
           88  ls-prev-op-none           value space.            

      *> Number of quoted pairs in a source code line.
       01  ls-quote-table.
           05  ls-num-quote-pairs       pic 9(4).
           05  ls-quote-location        occurs 0 to 9999 times 
                                        depending on ls-num-quote-pairs
                                        indexed by ls-quote-idx.
               10  ls-q-start-idx       pic 9(4) comp.
               10  ls-q-end-idx         pic 9(4) comp.      

       01  ls-quote-type-sw             pic a value 'E'.
           88  ls-quote-type-start      value 'S'.
           88  ls-quote-type-end        value 'E'.

       01  ls-quote-delimiter           pic a.

       01  ls-is-in-quote-sw            pic a value 'N'.
           88  ls-is-in-quote           value 'Y'.
           88  ls-is-not-in-quote       value 'N'.

       linkage section.       

       01  l-src-code-str                pic x(1024).       

      * copy "copybooks/linkage_section/l_variable_table.cpy".
           

       procedure division using l-src-code-str.   

       main-procedure.

      *> NOTE: Current implementation only reads equations from left to
      *>       right. No orders of operations are followed. That'll have
      *>       to be a later 'TODO'. 


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

           move ls-assignment-dest to ls-variable-name 
           call "get-variable" using 
               ls-variable ls-get-variable-return-code
           end-call 

      *> If not found, allocate a new variable before assignment.      
           if ls-get-variable-return-code = 0 then 
               perform allocate-new-variable
           else 
               if ls-type-string then 
                   set ls-assign-type-string to true 
               else 
                   set ls-assign-type-num to true 
               end-if            
           end-if 

      *> Find quote locations in assignment statement.
           move 1 to ls-temp-param-pointer
           perform until 
               ls-temp-param-pointer > length(ls-temp-param-buffer)

               unstring ls-temp-param-buffer 
                   delimited by '"'
                   into ls-temp-param-value
                   delimiter in ls-quote-delimiter
                   with pointer ls-temp-param-pointer
               end-unstring 
               
               if ls-quote-delimiter = '"' then 
                   if ls-quote-type-end then 
                       set ls-quote-type-start to true 
                   
                       add 1 to ls-num-quote-pairs
                       move ls-temp-param-pointer 
                       to ls-q-start-idx(ls-num-quote-pairs)

                       call "logger" using concatenate(
                           "**** QUOTE START: " ls-temp-param-pointer)
                       end-call 
                   else 
                       set ls-quote-type-end to true 

                       move ls-temp-param-pointer 
                       to ls-q-end-idx(ls-num-quote-pairs)
                       call "logger" using concatenate(
                           "**** QUOTE END: " ls-temp-param-pointer)
                       end-call 
                   end-if 
               end-if 
           end-perform 
               

      *> Break down the assignment statement by parts and process as
      *> needed. Delimiters found in quotes are ignored and appended
      *> to current quoted string.
           move 1 to ls-temp-param-pointer
           move trim(ls-temp-param-buffer) to ls-temp-param-buffer
           move spaces to ls-latest-operator
           move spaces to ls-temp-param-value            
           set ls-is-first-value to true 
           
           perform until 
               ls-temp-param-pointer > length(ls-temp-param-buffer)
               
   
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
               call "logger" using ls-prev-operator

               set ls-is-not-in-quote to true 

      *> Check if delimiter in quotes, if so, append to current working
      *> string data.
               perform varying ls-quote-idx from 1 by 1 
               until ls-quote-idx > ls-num-quote-pairs
                   if ls-temp-param-pointer > 
                       ls-q-start-idx(ls-quote-idx)
                       and ls-temp-param-pointer <
                       ls-q-end-idx(ls-quote-idx) 
                   then 
                       set ls-is-in-quote to true 
                       call "logger" using concatenate(
                           "**** IN QUOTE!: " ls-temp-param-pointer)
                       end-call 
                       string 
                           trim(ls-temp-param-value)
                           ls-latest-operator
                           into ls-temp-param-value
                       end-string 
                       exit perform 
                   end-if 
               end-perform 

               call "logger" using concatenate(
                   "++++++ ENTERING CALCULATE RUNNING VALUE WITH: "                 
                   ls-temp-param-value)
               end-call 
               perform calculate-running-assignment-value                         

               if ls-is-not-in-quote then 
                   move ls-latest-operator to ls-prev-operator
               end-if 

           end-perform


           
      *> Assign new value to variable            
           if ls-assign-type-num then 
               move ls-running-assign-val-num 
                   to ls-running-assign-val-num-disp

               move ls-running-assign-val-num
                   to ls-variable-value-num

               move ls-variable-value-num
                   to ls-variable-value-num-disp

               call "logger" using concatenate(
                   "ASSIGNMENT :: Number value. New value: "
                   ls-variable-value-num-disp
                   " : from: " ls-running-assign-val-num-disp)
               end-call                                              
           end-if 

                 
           if ls-assign-type-string then 

               call "logger" using concatenate(
                   "ASSIGNMENT :: New raw assignment value: "
                   ls-running-assign-val)
               end-call 
            
               move trim(ls-running-assign-val)
                   to ls-variable-value      
            
           end-if 
           
           call "set-variable" using ls-variable 

           call "logger" using concatenate(
               "ASSIGNMENT :: variable name: " 
               trim(ls-variable-name)
               " new value: " trim(ls-variable-value)
               " type: " ls-variable-type)      
           end-call                     

           goback. 





       calculate-running-assignment-value.
               
      *> Check to see if right hand of assignment is variable. If so,
      *> substitute the correct value in its place.           
           move ls-temp-param-value to ls-temp-variable-name 

           call "get-variable" using 
               ls-temp-variable ls-get-variable-return-code
           end-call 

           if ls-get-variable-return-code > 0 then 
               call "logger" using concatenate(
                   "ASSIGNMENT :: Found righthand side variable:" 
                   trim(ls-temp-variable-name)
                   " value: " 
                   trim(ls-temp-variable-value)
                   " num val: " 
                   ls-temp-variable-value-num)
               end-call 
      
               if ls-temp-type-integer then 
                   move ls-temp-variable-value-num
                   to ls-temp-param-value 
                           
               else 
                   move ls-temp-variable-value 
                   to ls-temp-param-value                            
               end-if  
           end-if 
              
           call "logger" using ls-running-assign-val-type-sw

           if ls-assign-type-string then 

               move trim(ls-temp-param-value) to ls-temp-param-value

      *>         Check if value INKEY$
               if upper-case(ls-temp-param-value) = ws-inkey then 
                   move spaces to ls-temp-param-value
                   move function inkey-func to ls-temp-inkey-ret-val
                   string 
                       '"'
                       trim(ls-temp-inkey-ret-val)
                       '"'
                       into ls-temp-param-value
                   end-string 
                    call "logger" using "**************INKEY**********"
                   call "logger" using ls-temp-param-value
               end-if 

      *>           Check for CHR$
               if upper-case(ls-temp-param-value(1:length(ws-chr)))
                   = ws-chr
               then
                   move ascii-code-to-char(ls-temp-param-value)  
                   to ls-temp-chr-check-string
                   move spaces to ls-temp-param-value 
                   string 
                       '"' 
                       trim(ls-temp-chr-check-string)
                       '"'
                       into ls-temp-param-value
                   end-string 
               end-if 

               if ls-is-first-value then 
                   move ls-temp-param-value 
                   to ls-running-assign-val
                   set ls-is-not-first-value to true 

                   call "logger" using concatenate(
                       "STRING FIRST VALUE: " ls-running-assign-val)
                   end-call 
               else 
                   if ls-prev-op-add then   

                       call "logger" using concatenate(
                           "COMBININING: " trim(ls-running-assign-val)
                           " WITH: " trim(ls-temp-param-value))
                       end-call 

      *>               Remove leading quote of new value.
                       if ls-temp-param-value(1:1) = '"' then 
                           move ls-temp-param-value(2:) 
                           to ls-temp-param-value 
                       end-if 

      *>               Find location of trailing quote of running value.
      *>               NOTE: May cause 'undefined' behavior if strings
      *>                     are not enclosed correctly with quotes.
                       move zero to ls-space-count
                       inspect reverse(ls-running-assign-val)
                           tallying ls-space-count 
                           for leading spaces 
                       
                       compute ls-trailing-quote-idx = 
                           length(ls-running-assign-val) 
                           - ls-space-count  
                       end-compute 

                       if ls-trailing-quote-idx = 0 then 
                           add 1 to ls-trailing-quote-idx
                       end-if                                                     

      *>               Move new value to location of previous trailing quote
                       move ls-temp-param-value 
                       to ls-running-assign-val(ls-trailing-quote-idx:)
     
                       call "logger" using concatenate(
                           "END VALUE: " trim(ls-running-assign-val))
                       end-call 
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
           exit paragraph.



       allocate-new-variable.
           call "logger" using concatenate(
               "ASSIGNMENT :: No variables exist yet for: "
               trim(ls-assignment-dest) 
               " : allocating new variable.")
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
