      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-20
      * Last Modified: 2021-12-03
      * Purpose: Processes a single conditional statement and returns 
      *          if true (1) or false (0).
      *          This should be called from the conditional-statement-handler
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. conditional-processor.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       78  ws-equal-to                   value "=".
       78  ws-not-equal-to               value "<>".
       78  ws-greater-than               value ">".
       78  ws-less-than                  value "<".
       78  ws-greater-than-equal-to      value ">=".
       78  ws-less-than-equal-to         value "<=".

       local-storage section.

       copy "copybooks/local_storage/ls_variable.cpy".         

       01  ls-num-parts-disp                  pic 9(4).

       01  ls-statement-to-process            pic x(1024).

       01  ls-conditional-parts-table.
           05  ls-num-parts                   pic 9(4) comp value zero.
           05  ls-statement-part              occurs 0 to 9999 times 
                                              depending on ls-num-parts
                                              indexed by ls-parts-idx.
               10  ls-part-value              pic x(1024) value spaces.
               10  ls-part-value-num          pic S9(16) value zeros.
               10  ls-part-type               pic a(8) value spaces.                   
                   88  ls-part-type-integer   value "INTEGER".
                   88  ls-part-type-string    value "STRING".
                   88  ls-part-type-operator  value "OPERATOR".

       01  ls-temp-str-comp-1                 pic x(1024).
       01  ls-temp-str-comp-2                 pic x(1024).

       01  ls-unstring-idx-pointer            pic 9(4) comp.       
       01  ls-parts-end-idx                   usage index.

       01  ls-temp-statement-value            pic x(1024) value spaces.
       01  ls-delimiter-value                 pic xx. 

       01  ls-sub-val-with-var-sw             pic a value 'N'.
           88  ls-sub-val-with-var            value 'Y'.
           88  ls-not-sub-val-with-var        value 'N'.

       01  ls-conditional-type-check-sw       pic a value 'N'.
           88  ls-conditional-check-numeric   value 'N'.
           88  ls-conditional-check-string    value 'S'.

       linkage section.       

       01  l-statement                        pic x(1024). 
                   
       01  l-return-code                      pic 9 value 0.
           88  l-return-code-false            value 0.
           88  l-return-code-true             value 1.


       procedure division using 
           l-statement l-return-code.   

       main-procedure.

           set l-return-code-false to true 

           move trim(l-statement) to ls-statement-to-process

           call "logger" using concatenate(
               "CONDITIONAL-PROCESSOR :: Evaluating statement: "
               trim(ls-statement-to-process))
           end-call 

           
           move 1 to ls-unstring-idx-pointer

           perform until 
           ls-unstring-idx-pointer > length(ls-statement-to-process)          

               move spaces to ls-temp-statement-value
               move spaces to ls-delimiter-value

      *>   NOTE: delimiter order matters. From most complex to least!
               unstring ls-statement-to-process       
                   delimited by 
                       ws-not-equal-to 
                       or ws-greater-than-equal-to 
                       or ws-less-than-equal-to
                       or ws-greater-than
                       or ws-less-than 
                       or ws-equal-to
                   into ls-temp-statement-value
                   delimiter in ls-delimiter-value
                   with pointer ls-unstring-idx-pointer
               end-unstring

               if ls-temp-statement-value = spaces then                    
                   exit perform 
               end-if 

               call "logger" using concatenate(
                   "CONDITIONAL-PROCESSOR :: statement part: "
                   trim(ls-temp-statement-value)
                   " : delimiter: " ls-delimiter-value)
               end-call 

               add 1 to ls-num-parts                            

               perform substitute-variable-val-if-exists

      *>   If not subbed with var, attempt to determine literal type.
               if ls-not-sub-val-with-var then 

                   move trim(ls-temp-statement-value) 
                   to ls-temp-statement-value
                   
                   if ls-temp-statement-value(1:1) = '"' then 
                       set ls-part-type-string(ls-num-parts) to true

                       move trim(ls-temp-statement-value) 
                           to ls-part-value(ls-num-parts)
                   else 
                       move ls-temp-statement-value 
                           to ls-part-value(ls-num-parts)

                       if trim(ls-temp-statement-value) is numeric then 

                           set ls-part-type-integer(ls-num-parts) 
                               to true 
                           move ls-temp-statement-value 
                               to ls-part-value-num(ls-num-parts)
                       else 
                           call "logger" using concatenate(
                               "CONDITIONAL-PROCESSOR :: WARNING :" 
                               " Item: " 
                               trim(ls-temp-statement-value) 
                               " is not a defined variable or of"
                               " string or numeric type. Returning "
                               " FALSE.")
                           end-call 
                           set l-return-code-false to true 
                           goback 
                       end-if 
                   end-if 
               end-if 

      *>       Add operator if current part is delimited by one.
               if ls-delimiter-value not = spaces then 
                   add 1 to ls-num-parts
                   set ls-part-type-operator(ls-num-parts) to true 

                   move ls-delimiter-value
                       to ls-part-value(ls-num-parts)
               end-if 
           end-perform 

           move ls-num-parts to ls-num-parts-disp
           call "logger" using concatenate(
               "CONDITIONAL-PROCESSOR :: NUM PARTS: " ls-num-parts-disp)
           end-call 

      *> If only one value, if value zero, return false, otherwise true.
           if ls-num-parts = 1 then 

      *> TODO : In QuickBasic, this would be caught by the pre-parser
      *>        and syntax checker before even running. Need to decide
      *>        if should abrupt exit with error or just assume false
      *>        like current code below.
               if ls-part-type-string(1) then 
                   call "logger" using concatenate(
                       "CONDITIONAL-PROCESSOR :: Type mismatch. "
                       "Cannot determine boolean value of string. "
                       "value: " trim(ls-part-value(1))
                       " : Return code false.")
                   end-call 
                   set l-return-code-false to true 
                   goback 
               end-if 

               if ls-part-value-num(1) = 0 then 
                   set l-return-code-false to true 
               else 
                   set l-return-code-true to true 
               end-if 
               
               goback 
           end-if 

      *>   At this point, must be three parts to test conditional values
           if ls-num-parts not = 3 then 
               goback 
           end-if 

           set ls-parts-end-idx to ls-num-parts
           perform varying ls-parts-idx from 1 by 1 
           until ls-parts-idx > ls-parts-end-idx
           
               call "logger" using concatenate(
                   "CONDITIONAL-PROCESSOR :: Part type: " 
                   ls-part-type(ls-parts-idx) 
                   " Part value string: " 
                   trim(ls-part-value(ls-parts-idx))
                   " Part value integer: " 
                   trim(ls-part-value-num(ls-parts-idx)))
               end-call 

           end-perform 

      *>   TODO : Currently only handles one statement (no AND or OR)
      *>          Updated: the multiple statements will be handled by 
      *>                   the conditional statement handler which calls
      *>                   this.

           if ls-part-type-integer(1) and ls-part-type-integer(3) then 
               set ls-conditional-check-numeric to true 
           else 
               set ls-conditional-check-string to true 
           end-if 

           evaluate ls-part-value(2) 

               when ws-equal-to 
                   call "logger" using concatenate(
                       "CONDITIONAL-PROCESSOR :: checking if equal: "
                       trim(ls-part-value(1)) "=" 
                       trim(ls-part-value(3)) " :: or numeric: "
                       trim(ls-part-value-num(1)) "=" 
                       trim(ls-part-value-num(3) ))
                   end-call 
                   if ls-conditional-check-numeric then 
                       call "logger" using "NUMERIC COMP"
                       if ls-part-value-num(1) = ls-part-value-num(3) 
                       then 
                           set l-return-code-true to true 
                       end-if
                   else 
                       call "logger" using 
                           "CONDITIONAL-PROCESSOR :: STRING COMP" 
                       end-call 
                       move trim(ls-part-value(1)) to ls-temp-str-comp-1
                       move trim(ls-part-value(3)) to ls-temp-str-comp-2

                       if 
                           ls-temp-str-comp-1 = ls-temp-str-comp-2
                       then                               
                           set l-return-code-true to true 
                       end-if 
                   end-if 


               when ws-not-equal-to
                   if ls-conditional-check-numeric then 
                       if ls-part-value-num(1) 
                           not = ls-part-value-num(3) 
                       then 
                           set l-return-code-true to true 
                       end-if 
                   else 
                       if ls-part-value(1) not = ls-part-value(3) then 
                           set l-return-code-true to true 
                       end-if 
                   end-if 


               when ws-greater-than
                 if ls-conditional-check-numeric then 
                       if ls-part-value-num(1) 
                           > ls-part-value-num(3) 
                       then 
                           set l-return-code-true to true 
                       end-if 
                   else 
                       if ls-part-value(1) > ls-part-value(3) then 
                           set l-return-code-true to true 
                       end-if 
                   end-if 


               when ws-greater-than-equal-to
                 if ls-conditional-check-numeric then 
                       if ls-part-value-num(1) 
                           >= ls-part-value-num(3) 
                       then 
                           set l-return-code-true to true 
                       end-if 
                   else 
                       if ls-part-value(1) >= ls-part-value(3) then 
                           set l-return-code-true to true 
                       end-if 
                   end-if 


               when ws-less-than
                 if ls-conditional-check-numeric then 
                       if ls-part-value-num(1) 
                           < ls-part-value-num(3) 
                       then 
                           set l-return-code-true to true 
                       end-if 
                   else 
                       if ls-part-value(1) < ls-part-value(3) then 
                           set l-return-code-true to true 
                       end-if 
                   end-if 


               when ws-less-than-equal-to
                 if ls-conditional-check-numeric then 
                       if ls-part-value-num(1) 
                           <= ls-part-value-num(3) 
                       then 
                           set l-return-code-true to true 
                       end-if 
                   else 
                       if ls-part-value(1) <= ls-part-value(3) then 
                           set l-return-code-true to true 
                       end-if 
                   end-if  

               end-evaluate
           goback.



       substitute-variable-val-if-exists.

           set ls-not-sub-val-with-var to true                           
           
      *>   Check for declared variable or intrinsic function
           move ls-temp-statement-value to ls-variable-name 
           call "get-variable" using 
               ls-variable 
               ls-get-variable-return-code
           end-call 

           if ls-get-variable-return-code > 0 then 

               if ls-type-integer then 
                   set ls-part-type-integer(ls-num-parts) to true
                   move ls-variable-value-num
                       to ls-part-value-num(ls-num-parts)
               else
                   set ls-part-type-string(ls-num-parts) to true
                   move ls-variable-value 
                       to ls-part-value(ls-num-parts)
               end-if 
               set ls-sub-val-with-var to true

               call "logger" using concatenate(
                       "CONDITIONAL-PROCESSOR :: Using variable: " 
                       trim(ls-variable-name) 
                       " type: " ls-variable-type
                       " value: " trim(ls-variable-value)
                       " value num: " ls-variable-value-num)
               end-call 
           end-if 
               
           exit paragraph.

       end program conditional-processor.
