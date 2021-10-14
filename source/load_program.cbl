      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-13
      * Last Modified: 2021-10-13
      * Purpose: Loads BASIC program into memory.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. load-program.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.

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

       copy "copybooks/basic_keywords.cpy".

       01  ws-input-source-file-name  pic x(1024).

       01  ws-line-idx                pic 9(10) comp value 0.
       01  ws-line-idx-disp           pic 9(10) value 0.

       01  ws-colon-count             pic 99 value zero.
       01  ws-starting-pointer        pic 99 comp.
      
       
       local-storage section.
       
       01  ls-eof-sw                  pic a value 'N'.
           88  ls-eof                 value 'Y'.
           88  ls-not-eof             value 'N'.

       01  ls-source-data-temp        pic x(1024).      

       linkage section.    

       01  l-input-file-name        pic x(1024).

       01  l-source-data-table.
           05  l-num-lines           pic 9(10) comp value 0.
           05  l-source-data-read    pic x(1024) 
                                     occurs 0 to unbounded times 
                                     depending on l-num-lines. 

       01  l-list-program-sw         pic a.
           88  l-list-program        value 'Y'.
           88  l-not-list-program    value 'N'.

       procedure division using 
           l-input-file-name l-source-data-table
           l-list-program-sw.  

       main-procedure.

           if l-input-file-name = spaces then 
               call "logger" using 
                   "File name not specified. Nothing to load."
               end-call 
               goback
           end-if 

           move l-input-file-name to ws-input-source-file-name

           if l-list-program then 
               display spaces 
               display "-----------------------------"
               display 
                   "Reading contents of " 
                   trim(ws-input-source-file-name)               
               end-display
               display "-----------------------------"
           end-if 

           open input fd-basic-source-file

               perform until ls-eof 
               
                   read fd-basic-source-file                        
                   at end set ls-eof to true 
                   not at end 
                       perform load-source-code-data                       
                   end-read 

               end-perform 

           close fd-basic-source-file

           if l-list-program then 
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
               from 1 by 1 until ws-line-idx > l-num-lines

                   display 
                       "LINE " ws-line-idx ": " 
                       trim(l-source-data-read(ws-line-idx))
                   end-display

               end-perform 

               display "-----------------------------"
               display "Done printing read contents"
               display "-----------------------------"               
               display spaces 
           end-if 

           goback.


       load-source-code-data.

           if l-list-program then 
               display trim(f-source-code-line)
           end-if 

      *> Check to see if line is split by colons. 
           move zeros to ws-colon-count
           inspect f-source-code-line 
           tallying ws-colon-count for all ":"                       

           if ws-colon-count > 0 then 
               call "logger" using concatenate(
                   "Found " ws-colon-count " colons in line.")
               end-call 

      *> Unstring line into multiple in-memory lines.
               move 1 to ws-starting-pointer
          
               perform ws-colon-count times 
                   add 1 to l-num-lines

                   unstring f-source-code-line
                       delimited by ":" 
                       into ls-source-data-temp
                       with pointer ws-starting-pointer
                   end-unstring

                   move trim(ls-source-data-temp)
                       to l-source-data-read(l-num-lines)
               end-perform
                           
      *> Add any text that may be after last colon in new line
               if ws-starting-pointer > 1 then 
                   add 1 to l-num-lines

                   move trim(f-source-code-line(ws-starting-pointer:))
                       to l-source-data-read(l-num-lines)
               end-if 
                                                      
           else
               add 1 to l-num-lines
               move trim(f-source-code-line)
                   to l-source-data-read(l-num-lines)
           end-if 

           exit paragraph.                                  

       end program load-program.
