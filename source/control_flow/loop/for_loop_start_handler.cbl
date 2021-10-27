      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-26
      * Last Modified: 2021-10-26
      * Purpose: Process and handles FOR loop start lines.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. for-loop-start-handler.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       01  ws-for-loop-data-table.
           05  ws-num-for-loops          pic 9(4) comp.
           05  ws-for-loop-data          occurs 0 to 1000 times 
                                         depending on ws-num-for-loops.
               10  ws-for-loop-line      pic 9(10).
               10  ws-for-loop-var       pic x(1024).
               10  ws-for-loop-start-val pic 9(10).
               10  ws-for-loop-end-val   pic 9(10).
               10  ws-for-loop-step      pic S9(16).
               10  ws-for-loop-init      pic a.
                   88  ws-is-init        value 'Y'.
                   88  ws-not-init       value 'N'.
 
       local-storage section.    
       
       01  ls-line-to-process        pic x(1024).

       01  ls-conditional-ret-val    pic 9.

       01  ls-cur-line-num-disp      pic 9(10).

       01  ls-variable-temp-data.
           05  ls-var-name           pic x(16).
           05  ls-var-type           pic x(8).
           05  ls-var-value          pic x(1024).
           05  ls-var-value-num      pic 9(16).
           05  ls-var-ret-code       pic 9.
       
       01  ls-part-temp              pic x(1024).

       01  ls-for-loop-parts         pic x(1024) occurs 8 times.
       01  ls-parts-idx              pic 9 comp.
       01  ls-unstring-idx           pic 9(4) comp.

       01  ls-working-for-loop-idx   pic 9(4) comp.

       01  ls-loop-idx               pic 9(4) comp.

       01  ls-assignment-str         pic x(1024).

       01  ls-new-var-value          pic 9(16).
      * 01  ls-conditional-str        pic x(1024).
      * 01  ls-conditional-operator   pic x(3).
      
       linkage section.       

       01  l-src-code-str            pic x(1024). 
 
       01  l-cur-line-num            pic 9(10) comp.


       01  l-loop-boundary-table.
           05  l-num-loops           pic 9(4) comp value 0. 
           05  l-loop-data           occurs 0 to unbounded times
                                     depending on l-num-loops.               
               10  l-loop-start      pic 9(10). *>TODO Make comp 
               10  l-loop-end        pic 9(10).

       01  l-variable-table.
           05  l-num-variables           pic 9(4) comp.
           05  l-variables               occurs 0 to unbounded times
                                         depending on l-num-variables. 
               10  l-variable-type       pic x(8) value spaces.
                   88  l-type-integer    value "INTEGER".
                   88  l-type-string     value "STRING".
               10  l-variable-name       pic x(16) value spaces.
               10  l-variable-value      pic x(1024) value spaces.
               10  l-variable-value-num  redefines l-variable-value
                                         pic 9(16) value zeros.       
              

       procedure division using 
           l-src-code-str l-cur-line-num 
           l-loop-boundary-table l-variable-table.   

       main-procedure.

           call "logger" using concatenate(
               "FOR-LOOP-START-HANDLER :: Enter with line: "
               trim(l-src-code-str))
           end-call 

           if l-num-loops = 0 then 
               goback 
           end-if 

           move 1 to ls-unstring-idx
           move 1 to ls-parts-idx
           move l-src-code-str to ls-line-to-process

           perform until ls-unstring-idx > length(l-src-code-str)           
           or ls-parts-idx > 8 

               unstring ls-line-to-process
                   delimited by space 
                   into ls-for-loop-parts(ls-parts-idx)
                   with pointer ls-unstring-idx              
               end-unstring 

               

               call "logger" using ls-for-loop-parts(ls-parts-idx)

               evaluate ls-parts-idx 

                   when 2 *> variable name
                       perform set-variable-name

                   when 4 *> start value
                       perform assign-start-value

                   when 6 *> end value 
                       perform check-end-val-condition

                   when 8 *> step value
                       perform set-step-value

               end-evaluate 

               add 1 to ls-parts-idx 

               
           end-perform 

           compute ls-new-var-value = 
               numval(ls-var-value) +               
               ws-for-loop-step(ls-working-for-loop-idx)
           end-compute 

           move concatenate(
               trim(ws-for-loop-var(ls-working-for-loop-idx))
               " = " 
               ls-new-var-value)
               to ls-assignment-str

           call "assign-var" using 
               ls-assignment-str 
               l-variable-table
           end-call 


           goback.
           

       init-new-for-loop-record.
           add 1 to ws-num-for-loops

           move ws-num-for-loops to ls-working-for-loop-idx

           move ls-for-loop-parts(ls-parts-idx)
               to ws-for-loop-var(ls-working-for-loop-idx)

           set ws-not-init(ls-working-for-loop-idx) to true 
               
           exit paragraph.



       set-variable-name.

      *>   If no loops in memory, add new one with variable name.
           if ws-num-for-loops = 0 then 
               perform init-new-for-loop-record
               exit paragraph
           end-if 

      *>   Otherwise, find existing entry 
           perform varying ls-working-for-loop-idx from 1 by 1 
           until ls-working-for-loop-idx > l-num-loops 

               if ws-for-loop-var(ls-working-for-loop-idx) 
               = ls-for-loop-parts(ls-parts-idx)
               then 
                   set ws-is-init(ls-working-for-loop-idx) to true
                   exit paragraph
               end-if  
           end-perform 

      *>   if not found, add a new record.
           perform init-new-for-loop-record

           exit paragraph.


       assign-start-value.
           if ws-is-init(ls-working-for-loop-idx) then 
               exit paragraph 
           end-if 

           move ls-for-loop-parts(ls-parts-idx) 
               to ws-for-loop-start-val(ls-working-for-loop-idx) 

           move concatenate(
               trim(ws-for-loop-var(ls-working-for-loop-idx))
               " = "
               ws-for-loop-start-val(ls-working-for-loop-idx))
               to ls-assignment-str

           call "assign-var" using 
               ls-assignment-str 
               l-variable-table
           end-call 

           set ws-is-init(ls-working-for-loop-idx) to true 

           exit paragraph.



       check-end-val-condition.
           
           move ls-for-loop-parts(ls-parts-idx) 
               to ws-for-loop-end-val(ls-working-for-loop-idx) 


           move ws-for-loop-var(ls-working-for-loop-idx) to ls-var-name
           
           call "get-var-value" using 
               l-variable-table
               ls-var-name 
               ls-var-type 
               ls-var-value
               ls-var-ret-code
           end-call 

           call "logger" using concatenate(
               "FOR-LOOP-START-HANDLER :: "
               "var name: " trim(ls-var-name)
               " : var type: " ls-var-type
               " : var value: " trim(ls-var-value)
               " : ret code: " ls-var-ret-code)
           end-call 
           
           if ls-var-ret-code = 0 or ls-var-type not = "INTEGER" then 
               call "logger" using concatenate(
                   "FOR-LOOP-START-HANDLER :: Error : Failed to find "
                   " FOR loop iterator variable in variable table! : "
                   " Skipping to end of loop. : "
                   "var name: " trim(ls-var-name)
                   " : var type: " ls-var-type
                   " : var value: " trim(ls-var-value)
                   " : ret code: " ls-var-ret-code)
               end-call 
               perform set-current-line-to-loop-exit-and-go-back
           end-if 

           if ws-for-loop-start-val(ls-working-for-loop-idx) 
               > ws-for-loop-end-val(ls-working-for-loop-idx) 
           then  
               *> move " < " to ls-conditional-operator
               if numval(ls-var-value) 
                   < ws-for-loop-end-val(ls-working-for-loop-idx) 
               then 
                   perform set-current-line-to-loop-exit-and-go-back
               end-if                    
           else 
      *         move " > " to ls-conditional-operator
               if numval(ls-var-value) 
                   > ws-for-loop-end-val(ls-working-for-loop-idx) 
               then 
                   perform set-current-line-to-loop-exit-and-go-back
               end-if                    

           end-if 

      *     move function concatenate(
      *         trim(ws-for-loop-var(ls-working-for-loop-idx))
      *         ls-conditional-operator
      *         ws-for-loop-end-val(ls-working-for-loop-idx))
      *         to ls-conditional-str
               
      *     call "conditional-processor" using 
      *         ls-conditional-str 
      *         l-variable-table
      *         ls-conditional-ret-val
      *     end-call 
           

           exit paragraph.



       set-step-value.           
           if ls-for-loop-parts(ls-parts-idx) = spaces then 
               move 1 to ws-for-loop-step(ls-working-for-loop-idx) 
           else 
               move numval(ls-for-loop-parts(ls-parts-idx))
               to ws-for-loop-step(ls-working-for-loop-idx)                      
           end-if           
           exit paragraph.



       set-current-line-to-loop-exit-and-go-back.                     
           perform varying ls-loop-idx from 1 by 1 
           until ls-loop-idx > l-num-loops 
               if l-loop-start(ls-loop-idx) = l-cur-line-num then 
                   set l-loop-end(ls-loop-idx) to l-cur-line-num
                   exit perform 
               end-if 
           end-perform 
           goback.


       end program for-loop-start-handler.
