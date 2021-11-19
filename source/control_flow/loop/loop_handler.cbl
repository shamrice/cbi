      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-25
      * Last Modified: 2021-11-19
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
           
       01  ls-loop-idx               pic 9(4) comp.

       01  ls-conditional-ret-val    pic 9.

       01  ls-cur-line-num-disp      pic 9(5).

       01  ls-line-text              pic x(1024).

       linkage section.       

       01  l-src-code-str            pic x(1024). 
 
       01  l-cur-line-num            pic 9(5) comp.

       copy "copybooks/linkage_section/l_loop_boundary_table.cpy".
        

       procedure division using 
           l-src-code-str l-cur-line-num 
           l-loop-boundary-table.   

       main-procedure.

           call "logger" using concatenate(
               "LOOP-HANDLER :: Enter with line: "
               trim(l-src-code-str))
           end-call 

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

               when upper-case(ls-line-text(1:length(ws-loop-while)))
                   = ws-loop-while 
                   perform handle-loop-while-end
                   
               when upper-case(ls-line-text(1:length(ws-loop-until)))
                   = ws-loop-until 
                   perform handle-loop-until-end                                      

               when upper-case(trim(ls-line-text)) = ws-loop
                   perform handle-no-condition-loop-end
                   
               when upper-case(ls-line-text(1:length(ws-for)))
                   = ws-for 
                   call "for-loop-handler" using 
                       ls-line-text
                       l-cur-line-num
                       l-loop-boundary-table                       
                   end-call 

               when upper-case(ls-line-text(1:length(ws-next)))
                   = ws-next 
                   call "for-loop-end-handler" using 
                       ls-line-text
                       l-cur-line-num
                       l-loop-boundary-table                       
                   end-call 

           end-evaluate
           
           goback.



       handle-while-loop-start.               
           call "logger" using "WHILE :: Processing WHILE loop start"
               
      *>   Check to see if condition is valid before continuing.
           call "conditional-processor" using 
               ls-line-text(length(ws-while):)               
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
               call "logger" using concatenate(
                   "LOOP : checking end of " ls-loop-idx 
                   " : l-loop-end: " l-loop-end(ls-loop-idx) 
                   " : cur line: " ls-cur-line-num-disp)
               end-call 

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




       handle-loop-while-end.               
           call "logger" using "LOOP WHILE :: Processing loop end"
               
      *>   Check to see if condition is valid before continuing.
           call "conditional-processor" using 
               ls-line-text(length(ws-loop-while):)               
               ls-conditional-ret-val
           end-call 
               
           call "logger" using ls-conditional-ret-val

      *>   Set line to end if conditional statement check fails.
           if ls-conditional-ret-val = 1 then                    
               call "logger" using "LOOP WHILE :: VALUE TRUE!"
      
           *>     Find matching loop exit line and redirect there.
               perform varying ls-loop-idx from 1 by 1
               until ls-loop-idx > l-num-loops 

                   if l-loop-end(ls-loop-idx) = l-cur-line-num then 
                         
                       move l-loop-start(ls-loop-idx) to l-cur-line-num
                       exit perform 
                   end-if 
               end-perform
           end-if 
           
           exit paragraph.



       handle-loop-until-end.               
           call "logger" using "LOOP UNTIL :: Processing loop end"
               
      *>   Check to see if condition is valid before continuing.
           call "conditional-processor" using 
               ls-line-text(length(ws-loop-until):)               
               ls-conditional-ret-val
           end-call 
               
           call "logger" using ls-conditional-ret-val

      *>   Set line to end if conditional statement check fails.
           if ls-conditional-ret-val = 0 then                    
               call "logger" using "LOOP UNTIL :: VALUE FALSE!"
      
           *>     Find matching loop exit line and redirect there.
               perform varying ls-loop-idx from 1 by 1
               until ls-loop-idx > l-num-loops 

                   if l-loop-end(ls-loop-idx) = l-cur-line-num then 
                         
                       move l-loop-start(ls-loop-idx) to l-cur-line-num
                       exit perform 
                   end-if 
               end-perform
           end-if 
           
           exit paragraph.           


       end program loop-handler.
