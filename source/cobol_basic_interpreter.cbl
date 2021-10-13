      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-09
      * Last Modified: 2021-10-13
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
           file-control.                              
               select optional fd-basic-source-file
               assign to dynamic ws-input-source-file-name
               organization is line sequential.          

       data division.
       file section.

       fd  fd-basic-source-file.
       01  f-source-code-line               pic x(1024).     

       working-storage section.

      *> copy "screenio.cpy".
       copy "copybooks/basic_keywords.cpy".

       01  ws-crt-status. 
           05  ws-key1                       pic x. 
           05  ws-key2                       pic x. 
           05  filler                        pic x. 
           05  filler                        pic x.
       
       01  ws-input-source-file-name  pic x(1024) value "./test.bas".

       01  ws-line-idx                pic 9(10) comp value 0.
       01  ws-line-idx-disp           pic 9(10) value 0.

       01  ws-num-lines               pic 9(10) comp value 0.

       01  ws-source-data-temp        pic x(1024).
       01  ws-source-data-read        pic x(1024) 
                                      occurs 0 to 64000 times 
                                      depending on ws-num-lines.          

       01  ws-eof-sw                  pic a value 'N'.
           88  ws-eof                 value 'Y'.
           88  ws-not-eof             value 'N'.

       01  ws-run-program-sw          pic a value 'N'.
           88  ws-run-program         value 'Y'.
           88  ws-not-run-program     value 'N'.

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
       01  ws-colon-count             pic 99 value zero.
       01  ws-assignment-count        pic 9 value zero.

       01  ws-starting-pointer        pic 99 comp.

       01  ws-temp-param-values       pic x(1024) occurs 10 times.

       01  ws-temp-variable-type      pic x(10) value spaces.

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
                                      


       01  is-valid-variable-sw       pic a value 'N'.
           88  ws-is-valid-variable   value 'Y'.
           88  ws-not-valid-variable  value 'N'.

       01  ws-text-colors.
           05  ws-text-fg-color          pic 99 value 7.
           05  ws-text-bg-color          pic 99 value 0.
           05  ws-text-fg-highlight-sw   pic a value 'N'.
               88  ws-text-fg-highlight  value 'Y'.
               88  ws-text-fg-lowlight   value 'N'.
        

       procedure division.
       set environment 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
       set environment 'COB_SCREEN_ESC'        TO 'Y'.
      
       main-procedure.
           display spaces 
           display "-----------------------------"
           display 
               "Reading contents of " 
               trim(ws-input-source-file-name)               
           end-display
           display "-----------------------------"

           open input fd-basic-source-file

               perform until ws-eof 
               
                   read fd-basic-source-file                        
                   at end set ws-eof to true 
                   not at end 
                       display trim(f-source-code-line)

                       inspect f-source-code-line 
                       tallying ws-colon-count for all ":"                       

                       if ws-colon-count > 0 then 
                           call "logger" using concatenate(
                               "Found " ws-colon-count 
                               " colons in line.")
                           end-call 
                           move 1 to ws-starting-pointer
                           perform ws-colon-count times 
                               add 1 to ws-num-lines

                               unstring f-source-code-line
                                   delimited by ":" 
                                   into 
                                   ws-source-data-temp
                                   with pointer ws-starting-pointer
                               end-unstring

                               move trim(ws-source-data-temp)
                                   to ws-source-data-read(ws-num-lines)
                           end-perform
                           
                           if ws-starting-pointer > 1 then 
                               add 1 to ws-num-lines
                               move trim(
                               f-source-code-line(ws-starting-pointer:))
                               to ws-source-data-read(ws-num-lines)
                           end-if 
                           *>add ws-colon-count to ws-num-lines
                           *>add 1 to ws-num-lines
                           move zeros to ws-colon-count
                       else
                           add 1 to ws-num-lines
                           move trim(f-source-code-line)
                               to ws-source-data-read(ws-num-lines)
                       end-if 


                       
                   end-read 

               end-perform 

           close fd-basic-source-file

           display "-----------------------------"
           display 
               "Done reading file: " 
               trim(ws-input-source-file-name)
           end-display 
           display "-----------------------------"
           display spaces 
           display spaces        
           display "-----------------------------"
           display "Printing read file contents:"
           display "----------------------------"

           perform varying ws-line-idx 
           from 1 by 1 until ws-line-idx > ws-num-lines

               display 
                   "LINE " ws-line-idx ": " 
                   trim(ws-source-data-read(ws-line-idx))
               end-display

           end-perform 

           display "-----------------------------"
           display "Done printing read contents"
           display "-----------------------------"               
           display spaces 
           
           display "Run program? [Y/N] " with no advancing
           accept ws-run-program-sw 

           if not ws-run-program then 
               display "Exiting..."
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
           = ws-end or ws-system then 
               call "logger" using "END or SYSTEM. Setting exit flag."
               set ws-exit-program to true 
               exit paragraph 
           end-if 

           if upper-case(ws-source-data-read(ws-line-idx)) = ws-cls then 
               display space blank screen 
               call "logger" using "CLS"
           end-if 

           if upper-case(
               ws-source-data-read(ws-line-idx)(1:5)) = ws-color
           then 
               call "set-cursor-color" using 
                   ws-source-data-read(ws-line-idx)
                   ws-text-colors
               end-call 
               
           end-if 

           if upper-case( 
               ws-source-data-read(ws-line-idx)(1:6)) = ws-locate 
           then 
               call "set-cursor-position" using 
                   ws-source-data-read(ws-line-idx)
                   ws-screen-position
               end-call       
              
           end-if 

           if upper-case(trim(
               ws-source-data-read(ws-line-idx)(1:5))) = ws-print
           then 
               move trim(ws-source-data-read(ws-line-idx)(6:))
                   to ws-temp-param-buffer

               *> check to see if printing variable value.
               if ws-temp-param-buffer(1:1) not = '"' then 
               
                   perform varying ws-temp-variable-idx
                   from 1 by 1 
                   until ws-temp-variable-idx > ws-num-variables

                       if upper-case(ws-temp-param-buffer)
                       = ws-variable-name(ws-temp-variable-idx) then 
                           move ws-variable-value(ws-temp-variable-idx)
                               to ws-temp-param-buffer
                           exit perform 
                       end-if 
                   end-perform 

               else 
                   inspect ws-temp-param-buffer 
                       replacing first '"' by space 
                   
                   inspect reverse(ws-temp-param-buffer)  
                       tallying ws-space-count for leading spaces
                   
                   move spaces to ws-temp-param-buffer(
                       length(ws-temp-param-buffer) - ws-space-count:)
               end-if 

               if ws-text-fg-highlight then 
                   display trim(ws-temp-param-buffer)
                       at ws-screen-position
                       highlight
                       foreground-color ws-text-fg-color  
                       background-color ws-text-bg-color       
                   end-display
               else 
                   display trim(ws-temp-param-buffer)
                       at ws-screen-position
                       foreground-color ws-text-fg-color 
                       background-color ws-text-bg-color        
                   end-display
               end-if 

               call "logger" using concatenate(
                   "PRINT :: location: " ws-screen-position
                   " colors: " ws-text-colors
                   " text: " trim(ws-temp-param-buffer))
               end-call 
       
               add 1 to ws-scr-row
               move 1 to ws-scr-col
           end-if 



           if upper-case(trim(
               ws-source-data-read(ws-line-idx)(1:3))) = ws-dim
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
