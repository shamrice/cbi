      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-26
      * Last Modified: 2021-11-19
      * Purpose: Process and handles FOR loop start/end lines.
      *          Main entry point handles loop start & top. 
      *          'for-loop-end-handler' entry point handles loop ending
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. for-loop-handler.

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
                                         depending on ws-num-for-loops
                                         indexed by 
                                         ls-working-for-loop-idx.
               10  ws-for-loop-line      pic 9(5).
               10  ws-for-loop-var       pic x(1024).
               10  ws-for-loop-start-val pic 9(16).
               10  ws-for-loop-end-val   pic 9(16).
               10  ws-for-loop-step      pic S9(16).
               
       local-storage section.    
       
       copy "copybooks/local_storage/ls_variable.cpy".  

       01  ls-line-to-process        pic x(1024).       

       01  ls-cur-line-num-disp      pic 9(5).            

       01  ls-for-loop-parts         pic x(1024) occurs 8 times
                                     indexed by ls-parts-idx.
      
       01  ls-unstring-idx           pic 9(4) comp.      

       01  ls-loop-idx               pic 9(4) comp.

       01  ls-assignment-str         pic x(1024).

       01  ls-new-var-value          pic 9(16).
 
       01  ls-next-var-name          pic x(1024).

       linkage section.       

       01  l-src-code-str            pic x(1024). 
 
       01  l-cur-line-num            pic 9(5) comp.

       copy "copybooks/linkage_section/l_loop_boundary_table.cpy".


       procedure division using 
           l-src-code-str l-cur-line-num 
           l-loop-boundary-table.   

       main-procedure.

           call "logger" using concatenate(
               "FOR-LOOP-START-HANDLER :: Enter with line: "
               trim(l-src-code-str))
           end-call 

           if l-num-loops = 0 then 
               goback 
           end-if 

           move 1 to ls-unstring-idx
           set ls-parts-idx to 1
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

               set ls-parts-idx up by 1                
               
           end-perform 

           goback.
           


       init-new-for-loop-record.
           add 1 to ws-num-for-loops

      *     move ws-num-for-loops to ls-working-for-loop-idx
           set ls-working-for-loop-idx to ws-num-for-loops

           move upper-case(ls-for-loop-parts(ls-parts-idx))
               to ws-for-loop-var(ls-working-for-loop-idx)
               
           exit paragraph.



       set-variable-name.

      *>   If no loops in memory, add new one with variable name.           
           if ws-num-for-loops = 0 then 
               call "logger" using "CREATING NEW"
               perform init-new-for-loop-record
               exit paragraph
           end-if 
          
      *>   Otherwise, find existing entry 
           perform varying ls-working-for-loop-idx from 1 by 1 
           until ls-working-for-loop-idx > ws-num-for-loops 

           call "logger" using ws-for-loop-var(ls-working-for-loop-idx) 

               if ws-for-loop-var(ls-working-for-loop-idx) 
               = upper-case(ls-for-loop-parts(ls-parts-idx))
               then       
                   exit paragraph
               end-if  
           end-perform 

      *>   if not found, add a new record.
           perform init-new-for-loop-record

           exit paragraph.


       assign-start-value.
      *>   TODO : Start value can also be a variable, not just an int!

           move ls-for-loop-parts(ls-parts-idx) 
               to ws-for-loop-start-val(ls-working-for-loop-idx) 

           move concatenate(
               trim(
                   upper-case(ws-for-loop-var(ls-working-for-loop-idx)))
               " = "
               ws-for-loop-start-val(ls-working-for-loop-idx))
               to ls-assignment-str

           call "assign-var" using 
               ls-assignment-str                
           end-call 

           exit paragraph.



       check-end-val-condition.

      *>   TODO : End value can also be a variable, not just an int!           

           move ls-for-loop-parts(ls-parts-idx) 
               to ws-for-loop-end-val(ls-working-for-loop-idx) 


           move ws-for-loop-var(ls-working-for-loop-idx) 
               to ls-variable-name
           
           call "get-variable" using 
               ls-variable 
               ls-get-variable-return-code
           end-call 

           call "logger" using concatenate(
               "FOR-LOOP-START-HANDLER :: "
               "var name: " trim(ls-variable-name)
               " : var type: " ls-variable-type
               " : var value: " trim(ls-variable-value)
               " : var value num: " ls-variable-value-num
               " : ret code: " ls-get-variable-return-code)
           end-call 
           
           if ls-get-variable-return-code = 0 or ls-type-string then 
               call "logger" using concatenate(
                   "FOR-LOOP-START-HANDLER :: Error : Failed to find "
                   " FOR loop iterator variable in variable table! : "
                   " Skipping to end of loop. : "
                   "var name: " trim(ls-variable-name)
                   " : var type: " ls-variable-type
                   " : var value: " trim(ls-variable-value)
                   " : var value num: " ls-variable-value-num
                   " : ret code: " ls-get-variable-return-code)
               end-call 
               perform set-current-line-to-loop-exit-and-go-back
           end-if 

           if ws-for-loop-start-val(ls-working-for-loop-idx) 
               > ws-for-loop-end-val(ls-working-for-loop-idx)                
           then                 
               if ls-variable-value-num
                   < ws-for-loop-end-val(ls-working-for-loop-idx) 
               then 
                   perform set-current-line-to-loop-exit-and-go-back
               end-if                    
           else       
               if ls-variable-value-num
                   > ws-for-loop-end-val(ls-working-for-loop-idx) 
               then 
                   perform set-current-line-to-loop-exit-and-go-back
               end-if                    

           end-if 

           exit paragraph.



       set-step-value.  
       *>  TODO : STEP value can also be variable not just int!

           if ls-for-loop-parts(ls-parts-idx) = spaces then 
               move 1 to ws-for-loop-step(ls-working-for-loop-idx) 
           else 
               move numval(ls-for-loop-parts(ls-parts-idx))
               to ws-for-loop-step(ls-working-for-loop-idx)                      
           end-if           
           exit paragraph.



       set-current-line-to-loop-exit-and-go-back.
           call "logger" using concatenate(
               "FOR-LOOP-START-HANDLER :: Exiting for loop. "
               "Setting line num to end of for loop " 
               ls-working-for-loop-idx)
           end-call

           perform varying ls-loop-idx from 1 by 1 
           until ls-loop-idx > l-num-loops 
               if l-loop-start(ls-loop-idx) = l-cur-line-num then 
                   move l-loop-end(ls-loop-idx) to l-cur-line-num
                   exit perform 
               end-if 
           end-perform 
           goback.




      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-27
      * Last Modified: 2021-10-28
      * Purpose: Entry point for handling 'NEXT {var}' syntax of 
      *          FOR...NEXT loops. Increments iteratator by STEP amount
      *          then checks if loop should continue or not. If not, 
      *          exits loop. 
      ******************************************************************
       entry "for-loop-end-handler" using 
           l-src-code-str l-cur-line-num 
           l-loop-boundary-table.

           call "logger" using concatenate(
               "FOR-LOOP-END-HANDLER :: Enter handler with line: " 
               trim(l-src-code-str))
           end-call 

      *>   Get for loop iterator variable name from NEXT statement           
           move upper-case(l-src-code-str) to ls-line-to-process

           inspect ls-line-to-process
           replacing all ws-next by spaces 

           move trim(ls-line-to-process) to ls-next-var-name
            
      *>   Find existing loop entry to get IDX in memory
           perform varying ls-working-for-loop-idx from 1 by 1 
           until ls-working-for-loop-idx > ws-num-for-loops 

               if ws-for-loop-var(ls-working-for-loop-idx) 
               = ls-next-var-name
               then  
                   call "logger" using concatenate(
                       "FOR-LOOP-END-HANDLER :: Found FOR loop info "
                       " in for loop data table for iterator: " 
                       trim(ls-next-var-name))
                   end-call 
                   exit perform
               end-if  
           end-perform 

      *>   If can't be found, leave the loop. 
           if ls-working-for-loop-idx > ws-num-for-loops then 
               call "logger" using concatenate(
                   "FOR-LOOP-END-HANDLER :: ERROR : Failed to find FOR "
                   "loop info for iterator: " trim(ls-next-var-name)
                   " : Possible NEXT without FOR. Exiting loop.")
               end-call 
               perform set-current-line-to-loop-exit-and-go-back
           end-if 
           
      *>   Get current value of variable.
           move ls-next-var-name to ls-variable-name 
           call "get-variable" using 
               ls-variable 
               ls-get-variable-return-code
           end-call 

           call "logger" using concatenate(
               "FOR-LOOP-END-HANDLER :: "
               "var name: " trim(ls-variable-name)
               " : var type: " ls-variable-type
               " : var value: " trim(ls-variable-value)
               " : var value num: " ls-variable-value-num 
               " : ret code: " ls-get-variable-return-code)
           end-call 
           
           if ls-get-variable-return-code = 0 or ls-type-string then 
               call "logger" using concatenate(
                   "FOR-LOOP-END-HANDLER :: Error : Failed to find "
                   " FOR loop iterator variable in variable table! : "
                   " EXITING loop. : "
                   "var name: " trim(ls-variable-name)
                   " : var type: " ls-variable-type
                   " : var value: " trim(ls-variable-value)
                   " : var value num: " ls-variable-value-num 
                   " : ret code: " ls-get-variable-return-code)
               end-call 
               perform set-current-line-to-loop-exit-and-go-back
           end-if 

      *>   Assign new value to the iterator. Increasing by STEP amount.
           compute ls-new-var-value = 
               ls-variable-value-num +               
               ws-for-loop-step(ls-working-for-loop-idx)
           end-compute 

           move concatenate(
               trim(ws-for-loop-var(ls-working-for-loop-idx))
               " = " 
               ls-new-var-value)
               to ls-assignment-str

           call "assign-var" using 
               ls-assignment-str                
           end-call 

      *>   Check exit condition of loop. If not, return to top.
           if (ws-for-loop-step(ls-working-for-loop-idx) > 0 
                   and ls-new-var-value 
                   > ws-for-loop-end-val(ls-working-for-loop-idx))
               or 
               (ws-for-loop-step(ls-working-for-loop-idx) < 0 
                   and ls-new-var-value 
                   < ws-for-loop-end-val(ls-working-for-loop-idx))
           then 
               perform set-current-line-to-loop-exit-and-go-back
           else 
               perform varying ls-loop-idx from 1 by 1
               until ls-loop-idx > l-num-loops 

                   move l-cur-line-num to ls-cur-line-num-disp
                   call "logger" using concatenate(
                       "FOR-LOOP-END-HANDLER :: checking end of " 
                       ls-loop-idx 
                       " : l-loop-end: " l-loop-end(ls-loop-idx) 
                       " : cur line: " ls-cur-line-num-disp)
                   end-call 

                   if l-loop-end(ls-loop-idx) = l-cur-line-num then  
                       compute l-cur-line-num = 
                           l-loop-start(ls-loop-idx)
                       end-compute                        
                       move l-cur-line-num to ls-cur-line-num-disp
                       call "logger" using concatenate(
                           "FOR-LOOP-END-HANDLER :: Top of FOR loop "
                           "found. Redirecting to top of loop at: "
                           ls-cur-line-num-disp)
                       end-call                                                      
                       exit perform  
                   end-if 
               end-perform 
           end-if 
           goback.

       end program for-loop-handler.
