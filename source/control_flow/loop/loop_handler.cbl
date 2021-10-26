      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-25
      * Last Modified: 2021-10-26
      * Purpose: Directs control flow to proper entry and exit 
      *          processing if current line is loop related
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. loop-handler.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".
 
       local-storage section.    
           
       01  ls-loop-idx               pic 9(10) comp.

       01  ls-conditional-ret-val    pic 9.

       01  ls-cur-line-num-disp      pic 9(10).

       01  ls-line-text              pic x(1024).

       linkage section.       

       01  l-src-code-str            pic x(1024). 
 
       01  l-cur-line-num            pic 9(10) comp.


       01  l-loop-boundary-table.
           05  l-num-loops           pic 9(10) comp value 0. 
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

           if l-num-loops = 0 then 
               call "logger" using concatenate(
                   "LOOP-HANDLER :: No loops declared. Ignoring.")
               end-call 
               goback 
           end-if 

           move trim(l-src-code-str) to ls-line-text

           evaluate true 
               when upper-case(ls-line-text(1:length(ws-while))) 
                   = ws-while             
                   perform handle-while-loop-start

               when upper-case(ls-line-text(1:length(ws-wend)))
                   = ws-wend 
                   perform handle-no-condition-loop-end 

               when upper-case(ls-line-text(1:length(ws-do-while)))
                   = ws-do-while 
                   perform handle-do-while-loop-start 

               when upper-case(ls-line-text(1:length(ws-do-until)))
                   = ws-do-until
                   perform handle-do-until-loop-start 


               when trim(upper-case(ls-line-text)) = ws-loop
                   perform handle-no-condition-loop-end



           end-evaluate

           
           goback.



       handle-while-loop-start.               
           call "logger" using "WHILE :: Processing WHILE loop start"
               
      *>   Check to see if condition is valid before continuing.
           call "conditional-processor" using 
               ls-line-text(length(ws-while):)
               l-variable-table
               ls-conditional-ret-val
           end-call 
               
           call "logger" using ls-conditional-ret-val

      *>   Set line to end if conditional statement check fails.
           if ls-conditional-ret-val = 0 then                    
               call "logger" using "WHILE :: VALUE FALSE!"
      
           *>     Find matching loop exit line and redirect there.
               perform varying ls-loop-idx from 1 by 1
               until ls-loop-idx > l-num-loops 

                   if l-loop-start(ls-loop-idx) = l-cur-line-num then 
                         
                       move l-loop-end(ls-loop-idx) to l-cur-line-num
                       exit perform 
                   end-if 
               end-perform
           end-if 
           
           exit paragraph.



       handle-no-condition-loop-end.
      *> Iterate through loop table and find start position of current
      *> loop's end.
           perform varying ls-loop-idx from 1 by 1
           until ls-loop-idx > l-num-loops 
               
               if l-loop-end(ls-loop-idx) = l-cur-line-num then 
      *> -1 because app line counter will auto increment in main parse loop                   
                   compute l-cur-line-num = 
                       l-loop-start(ls-loop-idx) - 1
                   end-compute 

                   move l-cur-line-num to ls-cur-line-num-disp
                   call "logger" using concatenate(
                       "LOOP-HANDLER :: found loop end "
                       "redirecting to top of "
                       "the loop at line: " ls-cur-line-num-disp)
                   end-call 
                   exit perform 
               end-if 

           end-perform 

           exit paragraph.




       handle-do-while-loop-start.               
           call "logger" using "DO WHILE :: Processing loop start"
               
      *>   Check to see if condition is valid before continuing.
           call "conditional-processor" using 
               ls-line-text(length(ws-do-while):)
               l-variable-table
               ls-conditional-ret-val
           end-call 
               
           call "logger" using ls-conditional-ret-val

      *>   Set line to end if conditional statement check fails.
           if ls-conditional-ret-val = 0 then                    
               call "logger" using "DO WHILE :: VALUE FALSE!"
      
           *>     Find matching loop exit line and redirect there.
               perform varying ls-loop-idx from 1 by 1
               until ls-loop-idx > l-num-loops 

                   if l-loop-start(ls-loop-idx) = l-cur-line-num then 
                         
                       move l-loop-end(ls-loop-idx) to l-cur-line-num
                       exit perform 
                   end-if 
               end-perform
           end-if 
           
           exit paragraph.



      *> TODO : this, DO WHILE and WHILE start are all the same basically.
      *>        should be refactorable into a simplier way without 
      *>        so much duplication.
       handle-do-until-loop-start.               
           call "logger" using "DO UNTIL :: Processing loop start"
               
      *>   Check to see if condition is valid before continuing.
           call "conditional-processor" using 
               ls-line-text(length(ws-do-until):)
               l-variable-table
               ls-conditional-ret-val
           end-call 
               
           call "logger" using ls-conditional-ret-val

      *>   Set line to end if conditional statement check fails.
           if ls-conditional-ret-val not = 0 then                    
               call "logger" using "DO UNTIL :: VALUE TRUE!"
      
           *>     Find matching loop exit line and redirect there.
               perform varying ls-loop-idx from 1 by 1
               until ls-loop-idx > l-num-loops 

                   if l-loop-start(ls-loop-idx) = l-cur-line-num then 
                         
                       move l-loop-end(ls-loop-idx) to l-cur-line-num
                       exit perform 
                   end-if 
               end-perform
           end-if 
           
           exit paragraph.           




       end program loop-handler.
