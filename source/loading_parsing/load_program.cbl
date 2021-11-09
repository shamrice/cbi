      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-13
      * Last Modified: 2021-11-08
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
       01  f-source-code-line         pic x(1024).     

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       01  ws-input-source-file-name  pic x(1024).

       01  ws-line-idx                pic 9(5) comp value 0.
       01  ws-line-idx-disp           pic 9(5) value 0.

       01  ws-colon-count             pic 9(4) value zero.
       01  ws-starting-pointer        pic 9(4) comp.
      
       
       local-storage section.
       
       01  ls-source-code-line        pic x(1024).

       01  ls-eof-sw                  pic a value 'N'.
           88  ls-eof                 value 'Y'.
           88  ls-not-eof             value 'N'.

       01  ls-source-data-temp        pic x(1024).        

       01  ls-quote-count             pic 9(4) comp.

       01  ls-quote-pair-idx          pic 9(4) comp.

      *> Number of quoted pairs in a source code line.
       01  ls-quote-table.
           05  ls-num-quote-pairs     pic 9(4).
           05  ls-quote-location      occurs 0 to 9999 times 
                                      depending on ls-num-quote-pairs.
               10  ls-q-start-idx     pic 9(4).
               10  ls-q-end-idx       pic 9(4).

       01  ls-quote-type-sw           pic a value 'E'.
           88  ls-quote-type-start    value 'S'.
           88  ls-quote-type-end      value 'E'.

       01  ls-line-char-idx           pic 9(4) comp.

       01  ls-colon-in-quote-sw       pic a value 'N'.
           88  ls-colon-in-quote      value 'Y'.
           88  ls-colon-not-in-quote  value 'N'.

       01  ls-single-line-if-sw       pic a value 'N'.
           88  ls-single-line-if      value 'Y'.
           88  ls-not-single-line-if  value 'N'.

       01  ls-single-line-if-parts.
           05  ls-if-start-part       pic x(1024).
           05  ls-if-end-parts        pic x(1024).

       01  ls-if-count                pic 9(4).
       01  ls-then-count              pic 9(4).

       linkage section.    

       01  l-input-file-name         pic x(1024).

       01  l-source-data-table.
           05  l-num-lines           pic 9(5) comp value 0.
           05  l-source-data-read    pic x(1024) 
                                     occurs 0 to unbounded times 
                                     depending on l-num-lines. 

       copy "copybooks/linkage_section/l_loop_boundary_table.cpy".

       copy "copybooks/linkage_section/l_sub_boundary_table.cpy".

       copy "copybooks/linkage_section/l_if_boundary_table.cpy".

       copy "copybooks/linkage_section/l_line_label_boundary_table.cpy".


       01  l-list-program-sw         pic a.
           88  l-list-program        value 'Y'.
           88  l-not-list-program    value 'N'.

       procedure division using 
           l-input-file-name l-source-data-table
           l-loop-boundary-table l-sub-boundary-table
           l-if-boundary-table
           l-line-label-boundary-table
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
                       if l-list-program then 
                           display trim(f-source-code-line)
                       end-if                   
      *> TODO : there should be some process at load time that converts 
      *>        all keywords to uppercase while leaving the rest of the 
      *>        line alone.  
                       move f-source-code-line to ls-source-code-line
                       perform process-single-line-if
                       if not ls-single-line-if then                  
                           perform load-source-code-data
                       end-if 
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
               display "-------------------------------"
               display "Printing parsed file contents:"
               display "-------------------------------"

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



       process-single-line-if.

           set ls-not-single-line-if to true 
           move spaces to ls-single-line-if-parts
           move zeros to ls-if-count
           move zeros to ls-then-count 

           inspect upper-case(ls-source-code-line)
           tallying 
               ls-if-count for all ws-if 
               ls-then-count for all ws-then                
           
           if ls-if-count > 0 and ls-then-count > 0 then 
               call "logger" using "LOAD :: checking single line IF"
               
      *>   TODO: keywords need to be uppercased for this to work. Currently
      *>         anything but an uppercase THEN will be ignored.
               unstring ls-source-code-line
                   delimited by ws-then 
                   into ls-if-start-part ls-if-end-parts
               end-unstring

               call "logger" using ls-if-start-part
               call "logger" using ls-if-end-parts

      *>   If single line IF, break processing up into parts and load
      *>   them. Append END IF to the end of the IF new block.
               if ls-if-end-parts not = spaces then 
                   set ls-single-line-if to true 

                   move ls-if-start-part to ls-source-code-line
                   perform load-source-code-data

                   move ls-if-end-parts to ls-source-code-line
                   perform load-source-code-data

                   move ws-end-if to ls-source-code-line
                   perform load-source-code-data

               end-if 
           end-if 

           exit paragraph.       



       load-source-code-data. 

      *> Figure out quote start and end locations.   
           move zeros to ls-num-quote-pairs       
           inspect ls-source-code-line
           tallying ls-quote-count for all '"'.

           if ls-quote-count > 0 then 
               perform varying ls-line-char-idx from 1 by 1
               until ls-line-char-idx > length(ls-source-code-line) 

                   if ls-source-code-line(ls-line-char-idx:1) = '"' then 

                       if not ls-quote-type-start then 
                           add 1 to ls-num-quote-pairs
                           move ls-line-char-idx 
                               to ls-q-start-idx(ls-num-quote-pairs)
                           
                           set ls-quote-type-start to true 

                           call "logger" using concatenate(
                               "LOAD :: quote START at: " 
                               ls-q-start-idx(ls-num-quote-pairs)
                               " for line: " 
                               trim(ls-source-code-line)) 
                           end-call 
                       
                       else 
                           compute ls-q-end-idx(ls-num-quote-pairs) 
                               = ls-line-char-idx + 1
                           end-compute 

                           set ls-quote-type-end to true 

                           call "logger" using concatenate(
                               "LOAD :: quote END at: " 
                               ls-q-end-idx(ls-num-quote-pairs) 
                               " for line: " 
                               trim(ls-source-code-line))
                           end-call 
                                                     
                       end-if 
                   end-if 
               
               end-perform 
           end-if 

      *> Check to see if line is split by colons. 
           move zeros to ws-colon-count
           inspect ls-source-code-line 
           tallying ws-colon-count for all ":"                       

           if ws-colon-count > 0 then 
               call "logger" using concatenate(
                   "Found " ws-colon-count " colons in line.")
               end-call 

      *> Unstring line into multiple in-memory lines.
               move 1 to ws-starting-pointer
               
               perform ws-colon-count times

                   unstring ls-source-code-line
                       delimited by ":" 
                       into ls-source-data-temp
                       with pointer ws-starting-pointer
                   end-unstring

      *> Make sure that the colon is not in quoted text. 
                                      
                   set ls-colon-not-in-quote to true 

                   perform varying ls-quote-pair-idx from 1 by 1 
                   until ls-quote-pair-idx > ls-num-quote-pairs
                       if ws-starting-pointer > 
                           ls-q-start-idx(ls-quote-pair-idx)
                           and ws-starting-pointer 
                           < ls-q-end-idx(ls-quote-pair-idx)
                       then 
                           set ls-colon-in-quote to true                            
                           exit perform 
                       end-if 

                   end-perform 

      *> If not quoted, then generate new line.
                   if ls-colon-not-in-quote then 
                       add 1 to l-num-lines

                       move trim(ls-source-data-temp)
                          to l-source-data-read(l-num-lines)    
               
                       *> check if it's a line label.
                       call "parse-line-labels" using 
                           l-source-data-read(l-num-lines)
                           l-num-lines 
                           l-line-label-boundary-table
                       end-call   

                       perform run-parsing-sub-programs                                           
                   end-if 

               end-perform
                           
      *> Add any text that may be after last colon in new line
               if ws-starting-pointer > 1 and ls-colon-not-in-quote then 
                   add 1 to l-num-lines

                   move trim(ls-source-code-line(ws-starting-pointer:))
                       to l-source-data-read(l-num-lines)
               else *> colon exists in quoted text in the string. parse normal
                   add 1 to l-num-lines
                   move trim(ls-source-code-line)
                       to l-source-data-read(l-num-lines)
               end-if 
                                                      
           else
               add 1 to l-num-lines
               move trim(ls-source-code-line)
                   to l-source-data-read(l-num-lines)
           end-if 
            
           perform run-parsing-sub-programs

           exit paragraph.


       run-parsing-sub-programs.

           call "parse-loops" using 
               l-source-data-read(l-num-lines)
               l-num-lines 
               l-loop-boundary-table
           end-call 

           call "parse-subs" using 
               l-source-data-read(l-num-lines)
               l-num-lines 
               l-sub-boundary-table
           end-call  

           call "parse-ifs" using 
               l-source-data-read(l-num-lines)
               l-num-lines 
               l-if-boundary-table
           end-call  


      *>   Check for GOSUB returns. 
           if upper-case(trim(
               l-source-data-read(l-num-lines)(1:length(ws-return))
               )) = ws-return 
           then 
               call "parse-line-labels" using 
                   l-source-data-read(l-num-lines)
                   l-num-lines 
                   l-line-label-boundary-table
               end-call  
           end-if 

           exit paragraph.                                  

       end program load-program.
