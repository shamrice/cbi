      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-09
      * Last Modified: 2021-10-25
      * Purpose: BASIC interpretter written in COBOL      
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. cobol-basic-interpreter.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.
           crt status is ws-crt-status.

       input-output section.
     
       data division.
       file section.

       working-storage section.

      *> copy "screenio.cpy".
       copy "copybooks/basic_keywords.cpy".

       01  ws-crt-status. 
           05  ws-key1                       pic x. 
           05  ws-key2                       pic x. 
           05  filler                        pic x. 
           05  filler                        pic x.
       
       01  ws-input-source-file-name  pic x(1024) value spaces.

       01  ws-line-idx                pic 9(10) comp value 0.
       01  ws-line-idx-disp           pic 9(10) value 0.
       
       01  ws-loop-idx                pic 9(10) comp.
       01  ws-sub-idx                 pic 9(4) comp.

       01  ws-source-data-temp        pic x(1024).
       01  ws-source-data-table.
           05  ws-num-lines           pic 9(10) comp value 0.
           05  ws-source-data-read    pic x(1024) 
                                      occurs 0 to 64000 times 
                                      depending on ws-num-lines.       

       01  ws-run-program-sw          pic a value 'N'.
           88  ws-run-program         value 'Y'.
           88  ws-not-run-program     value 'N'.

       01  ws-list-program-sw         pic a value 'N'.
           88  ws-list-program        value 'Y'.
           88  ws-not-list-program    value 'N'.

       01  ws-exit-program-sw         pic a value 'N'.
           88  ws-exit-program        value 'Y'.
           88  ws-not-exit-program    value 'N'.

       01  ws-screen-position.
           05  ws-scr-row             pic 999 value 1.
           05  ws-scr-col             pic 999 value 1.            

       01  ws-temp-cmd-buffer         pic x(256).
       01  ws-temp-param-buffer       pic x(1024).

       01  ws-temp-sub-name           pic x(32).

       01  ws-space-count             pic 9(10) comp value zero.
       01  ws-comma-count             pic 9(10) comp value zero.
       01  ws-keyword-count           pic 9(10) comp value zero.
       
       01  ws-assignment-count        pic 9 comp value zero.

       01  ws-conditional-ret-val     pic 9 value 0.   
       01  ws-allocate-ret-val        pic 9 value 0.  
       01  ws-keyword-check-ret-val   pic 9 value 0. 
       
       01  ws-assert-check-val        pic x(1024) value spaces.

       01  ws-variable-table.
           05  ws-num-variables           pic 9(4) comp.
           05  ws-variables               occurs 0 to 1000 times
                                          depending on ws-num-variables. 
               10  ws-variable-type       pic x(8) value spaces.
                   88  ws-type-integer    value "INTEGER".
                   88  ws-type-string     value "STRING".
               10  ws-variable-name       pic x(16) value spaces.
               10  ws-variable-value      pic x(1024) value spaces.
               10  ws-variable-value-num  redefines ws-variable-value
                                          pic 9(16) value zeros.    
       01  ws-loop-boundary-table.
           05  ws-num-loops               pic 9(10) comp. 
           05  ws-loop-data               occurs 0 to 1000 times
                                          depending on ws-num-loops.               
               10  ws-loop-start          pic 9(10). *>TODO Make comp 
               10  ws-loop-end            pic 9(10).                                      

       01  ws-sub-boundary-table.
           05  ws-num-subs                pic 9(10) comp. 
           05  ws-sub-data                occurs 0 to 1000 times
                                          depending on ws-num-subs.    
               10  ws-sub-name            pic x(32).           
               10  ws-sub-start           pic 9(10). *>TODO Make comp 
               10  ws-sub-end             pic 9(10).  
               10  ws-sub-cur-nest        pic 9(4) value 0.
               10  ws-sub-last-call       pic 9(10) occurs 1000 times.
                                         *>idx of last call is cur nest.                  

       01  ws-text-colors.
           05  ws-text-fg-color           pic 99 value 7.
           05  ws-text-bg-color           pic 99 value 0.
           05  ws-text-fg-highlight-sw    pic a value 'N'.
               88  ws-text-fg-highlight   value 'Y'.
               88  ws-text-fg-lowlight    value 'N'.

       01  ws-command-line-args           pic x(2048).


       procedure division.
       set environment 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
       set environment 'COB_SCREEN_ESC'        TO 'Y'.
      
       main-procedure.
           
           display spaces 
           display "CBI - COBOL BASIC Interpreter"
           display "-----------------------------"
           display " By: Erik Eriksen"
           display "Url: https://github.com/shamrice/cbi"
           display space 

           accept ws-command-line-args from command-line
           call "command-line-parser" using 
               ws-command-line-args
               ws-input-source-file-name
               ws-list-program-sw
               ws-run-program-sw               
           end-call 
           
           call "load-program" using 
               ws-input-source-file-name 
               ws-source-data-table
               ws-loop-boundary-table
               ws-sub-boundary-table
               ws-list-program-sw
           end-call 
         
           if not ws-run-program then               
               stop run 
           end-if                  

           perform parse-and-run-program

           stop run.

       parse-and-run-program.

           perform varying ws-line-idx from 1 by 1 
           until ws-line-idx > ws-num-lines or ws-exit-program

               move spaces to ws-temp-cmd-buffer
               move spaces to ws-temp-param-buffer
              
               move zeros to ws-space-count
               move zeros to ws-comma-count    
               move zeros to ws-keyword-count   
               move zero to ws-assignment-count       

               move ws-line-idx to ws-line-idx-disp
               call "logger" using concatenate( 
                   "LINE " ws-line-idx-disp " :: " 
                   trim(ws-source-data-read(ws-line-idx)))                   
               end-call 

               if trim(
               ws-source-data-read(ws-line-idx)(1:1)) = ws-comment-tic
               or trim(
               ws-source-data-read(ws-line-idx)(1:3)) = ws-comment-rem
               or ws-source-data-read(ws-line-idx) = spaces 
               then 
                   call "logger" using 
                       "Comment or empty line, skipping."
                   end-call                    
               else 
                   perform process-line
               end-if               

           end-perform 

           exit paragraph.




       process-line.

           evaluate true 

               when upper-case(ws-source-data-read(ws-line-idx)) 
                   = ws-end or ws-system or ws-stop    

                   call "logger" using 
                       "END SYSTEM or STOP. Setting exit flag."
                   end-call 
                   set ws-exit-program to true 
                   exit paragraph 
           

               when upper-case(ws-source-data-read(ws-line-idx)) 
                   = ws-cls 

                   display space blank screen 
                   move 1 to ws-scr-col
                   move 1 to ws-scr-row 
                   call "logger" using "CLS"
               

               when upper-case(
                   ws-source-data-read(ws-line-idx)(1:length(ws-sleep))) 
                   = ws-sleep 

                   call "sleep-program" using 
                       ws-source-data-read(ws-line-idx)                
                   end-call 
                         

               when upper-case(
                   ws-source-data-read(ws-line-idx)(1:length(ws-color))) 
                   = ws-color
           
                   call "set-cursor-color" using 
                       ws-source-data-read(ws-line-idx)
                       ws-text-colors
                   end-call 
               

               when upper-case( 
                   ws-source-data-read(ws-line-idx)(1:length(ws-locate))
                   ) = ws-locate 
           
                   call "set-cursor-position" using 
                       ws-source-data-read(ws-line-idx)
                       ws-screen-position
                   end-call       
              

               when upper-case(
                   ws-source-data-read(ws-line-idx)(1:length(ws-print))) 
                   = ws-print
           
                   call "print-text" using 
                       ws-source-data-read(ws-line-idx)
                       ws-screen-position
                       ws-text-colors
                       ws-variable-table
                   end-call 
           
               when upper-case(
                   ws-source-data-read(ws-line-idx)(1:length(ws-input))) 
                   = ws-input
           
                   call "input-cmd" using 
                       ws-source-data-read(ws-line-idx)
                       ws-screen-position
                       ws-text-colors
                       ws-variable-table 
                   end-call 

               when upper-case(
                   ws-source-data-read(ws-line-idx)(1:length(ws-dim))) 
                   = ws-dim
           
                   call "allocate-var" using 
                       ws-source-data-read(ws-line-idx)
                       ws-variable-table 
                       ws-allocate-ret-val
                   end-call 

               when upper-case(
                   ws-source-data-read(ws-line-idx)(1:length(ws-call)))
                   = ws-call 
                       perform handle-call
                   

           end-evaluate 

           perform check-assign-value-to-variable
           perform check-and-handle-loop-end           
           perform check-and-handle-loop-start
           perform check-and-handle-sub-start 
           perform check-and-handle-sub-end

           exit paragraph.




       check-assign-value-to-variable.           

           unstring trim(ws-source-data-read(ws-line-idx))
               delimited by space 
               into ws-assert-check-val
           end-unstring

           call "is-keyword" using 
               ws-assert-check-val
               ws-keyword-check-ret-val
           end-call 

           if ws-keyword-check-ret-val = 1 then 
               exit paragraph 
           end-if 
           

           inspect ws-source-data-read(ws-line-idx) 
               tallying ws-assignment-count for all "="
           
           if ws-assignment-count = 1 then 
               call "assign-var" using 
                   ws-source-data-read(ws-line-idx) 
                   ws-variable-table
               end-call 
           end-if 


           exit paragraph.


      *> TODO : MOVE TO OWN SUB PROGRAM WITH END!
       check-and-handle-loop-start.

      *> Make sure there's loops and that the current line is a loop start
           if ws-num-loops = 0 then 
               exit paragraph
           end-if 
           
           if upper-case(
               ws-source-data-read(ws-line-idx)(1:length(ws-while))) 
               = ws-while  
           then
               *> TODO : move to sub program.
               call "logger" using "processing WHILE loop start"
               
      *>       Check to see if condition is valid before continuing.
               call "conditional-processor" using 
                   ws-source-data-read(ws-line-idx)(length(ws-while):)
                   ws-variable-table
                   ws-conditional-ret-val
               end-call 
               
               call "logger" using ws-conditional-ret-val

      *>       Reset line ot end if conditional statement check fails.
               if ws-conditional-ret-val = 0 then                    
                   call "logger" using "WHILE :: VALUE FALSE!"
      
           *>     Find matching loop exit line and redirect there.
                   perform varying ws-loop-idx from 1 by 1
                   until ws-loop-idx > ws-num-loops 

                       if ws-loop-start(ws-loop-idx) = ws-line-idx then 
                           
                           move ws-loop-end(ws-loop-idx) to ws-line-idx
                           exit perform 
                       end-if 
                   end-perform
               end-if 

           end-if


           exit paragraph.


      *> TODO : MOVE TO OWN SUB PROGRAM WITH START!
       check-and-handle-loop-end.

      *> Make sure there's loops and that the current line is a loop exit
           if ws-num-loops = 0 then 
               exit paragraph
           end-if 
           
           if upper-case(
               ws-source-data-read(ws-line-idx)(1:length(ws-wend))) 
               not = ws-wend 
           then
               exit paragraph
           end-if 

      *> Iterate through loop table and find start position of current
      *> loop's end.
           perform varying ws-loop-idx from 1 by 1
           until ws-loop-idx > ws-num-loops 
               
               if ws-loop-end(ws-loop-idx) = ws-line-idx then 
      *> -1 because app line counter will auto increment in main parse loop                   
                   compute ws-line-idx = ws-loop-start(ws-loop-idx) - 1

                   move ws-line-idx to ws-line-idx-disp
                   call "logger" using concatenate(
                       "PARSE :: found loop end, redirecting to top of "
                       "the loop at line: " ws-line-idx-disp)
                   end-call 
                   exit perform 
               end-if 

           end-perform 

           exit paragraph.



      *> TODO : Move to own sub program for handle in general of subs.
       check-and-handle-sub-start.

      *> Make sure there's subs 
           if ws-num-subs = 0 then 
               exit paragraph
           end-if 
           
           if upper-case(
               ws-source-data-read(ws-line-idx)(1:length(ws-sub))) 
               = ws-sub  
           then
               *> TODO : move to sub program.
               call "logger" using "processing SUB start"
               
               perform varying ws-sub-idx from 1 by 1
               until ws-sub-idx > ws-num-subs 
                   if ws-sub-start(ws-sub-idx) = ws-line-idx then 

                       if ws-sub-cur-nest(ws-sub-idx) = 0 then 
                           move ws-line-idx to ws-line-idx-disp
                           call "logger" using concatenate(
                               "PARSE :: SUBROUTINE start on line " 
                               ws-line-idx-disp
                               " has not been invoked yet. Skipping "
                               " to END SUB at line "
                               ws-sub-end(ws-sub-idx)) 
                           end-call 

                       *> -1 because program counter will add line at next loop
                           compute ws-line-idx =
                                ws-sub-end(ws-sub-idx) - 1    
                           end-compute 
                           
                           exit perform                        
                       end-if 

               end-perform        

           end-if

           exit paragraph.


      *> TODO : MOVE TO OWN SUB PROGRAM WITH START!
       check-and-handle-sub-end.
      *> Make sure there's subs and that the current line is a sub exit
           if ws-num-subs = 0 then 
               exit paragraph
           end-if 
           
           if upper-case(
               ws-source-data-read(ws-line-idx)(1:length(ws-end-sub))) 
               not = ws-end-sub 
           then               
               exit paragraph
           end-if 

      *>   Iterate through loop table and find last called line of
      *>   sub and subtract the nest index by 1.
           perform varying ws-sub-idx from 1 by 1
           until ws-sub-idx > ws-num-subs 
               
               if ws-sub-end(ws-sub-idx) = ws-line-idx then 
                   
                   
                   if ws-sub-cur-nest(ws-sub-idx) > 0 then 
                       move ws-sub-last-call(
                           ws-sub-idx, ws-sub-cur-nest(ws-sub-idx))
                           to ws-line-idx 

                       subtract 1 from ws-sub-cur-nest(ws-sub-idx) 
                       
                       move ws-line-idx to ws-line-idx-disp
                       call "logger" using concatenate(
                           "PARSE :: found END SUB. Redirecting to last"
                           " line to call sub: " ws-line-idx-disp)
                       end-call 
                   else 
                       call "logger" using concatenate(
                           "PARSE :: found END SUB. Current SUB was not"
                           " invoked, so ignoring and moving to next "
                           " line in the program.")
                       end-call                    
                   end-if                    
                   
                   exit perform 
               end-if 

           end-perform 

           exit paragraph.


      *> TODO : MOVE TO OWN SUB PROGRAM!
       handle-call.

           call "logger" using "ENTER CALL HANDLER"

           if ws-num-subs = 0 then 
               call "logger" using concatenate(
                   "PARSE :: CALL without any SUBs declared. Ignoring.")
               end-call 
               exit paragraph
           end-if 

           move trim(upper-case(
               ws-source-data-read(ws-line-idx)(length(ws-call):)))
                to ws-temp-sub-name

           call "logger" using ws-temp-sub-name

           perform varying ws-sub-idx from 1 by 1 
           until ws-sub-idx > ws-num-subs 
               
               if ws-sub-name(ws-sub-idx) = ws-temp-sub-name then 

               *> Add to nest idx (invoke count) and keep track of this
               *> as source called line. Then redirect processing to sub
                   add 1 to ws-sub-cur-nest(ws-sub-idx)
                   
                   move ws-line-idx 
                   to ws-sub-last-call(
                       ws-sub-idx, 
                       ws-sub-cur-nest(ws-sub-idx))

                   move ws-sub-start(ws-sub-idx) to ws-line-idx 

                   move ws-line-idx to ws-line-idx-disp
                   call "logger" using concatenate(
                       "HANDLE-CALL :: found sub: " 
                       trim(ws-temp-sub-name)
                       " : moving line idx to: " ws-line-idx-disp)
                   end-call 
                   exit perform 
               end-if 

           end-perform 
           

           exit paragraph.

       end program cobol-basic-interpreter.
