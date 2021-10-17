      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-09
      * Last Modified: 2021-10-17
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

       01  ws-space-count             pic 9(10) value zero.
       01  ws-comma-count             pic 9(10) value zero.
       01  ws-keyword-count           pic 9(10) value zero.
       
       01  ws-assignment-count        pic 9 value zero.
              

       01  ws-temp-variable-idx       pic 9(4) comp value 0.
       
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
                                      

       01  ws-text-colors.
           05  ws-text-fg-color          pic 99 value 7.
           05  ws-text-bg-color          pic 99 value 0.
           05  ws-text-fg-highlight-sw   pic a value 'N'.
               88  ws-text-fg-highlight  value 'Y'.
               88  ws-text-fg-lowlight   value 'N'.

       01  ws-command-line-args          pic x(2048).


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

           if upper-case(ws-source-data-read(ws-line-idx)) 
           = ws-end or ws-system or ws-stop then 
               call "logger" using 
                   "END SYSTEM or STOP. Setting exit flag."
               end-call 
               set ws-exit-program to true 
               exit paragraph 
           end-if 

           if upper-case(ws-source-data-read(ws-line-idx)) = ws-cls then 
               display space blank screen 
               move 1 to ws-scr-col
               move 1 to ws-scr-row 
               call "logger" using "CLS"
           end-if 

           if upper-case
           (ws-source-data-read(ws-line-idx)(1:length(ws-sleep))) 
           = ws-sleep then 
               call "sleep-program" using 
                   ws-source-data-read(ws-line-idx)                
               end-call 
           end-if                

           if upper-case(
               ws-source-data-read(ws-line-idx)(1:length(ws-color))) 
               = ws-color
           then 
               call "set-cursor-color" using 
                   ws-source-data-read(ws-line-idx)
                   ws-text-colors
               end-call 
               
           end-if 

           if upper-case( 
               ws-source-data-read(ws-line-idx)(1:length(ws-locate))) 
               = ws-locate 
           then 
               call "set-cursor-position" using 
                   ws-source-data-read(ws-line-idx)
                   ws-screen-position
               end-call       
              
           end-if 

           if upper-case(
               ws-source-data-read(ws-line-idx)(1:length(ws-print))) 
               = ws-print
           then 
               call "print-text" using 
                   ws-source-data-read(ws-line-idx)
                   ws-screen-position
                   ws-text-colors
                   ws-variable-table
               end-call 
           end-if 

      *     if upper-case(
      *         ws-source-data-read(ws-line-idx)(1:length(ws-input))) 
      *         = ws-input
      *     then 
      *         call "input-cmd" using 
      *             ws-source-data-read(ws-line-idx)
      *             ws-variable-table 
      *         end-call 
      *     end-if 

           if upper-case(
               ws-source-data-read(ws-line-idx)(1:length(ws-dim))) 
               = ws-dim
           then 
               call "allocate-var" using 
                   ws-source-data-read(ws-line-idx)
                   ws-variable-table 
               end-call 

           end-if 

           perform check-assign-value-to-variable
           
           exit paragraph.




       check-assign-value-to-variable.           

           inspect ws-source-data-read(ws-line-idx) 
               tallying ws-assignment-count for all "="
           
           if ws-assignment-count = 1 then 
               call "assign-var" using 
                   ws-source-data-read(ws-line-idx) 
                   ws-variable-table
               end-call 
           end-if 


           exit paragraph.

       end program cobol-basic-interpreter.
