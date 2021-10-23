      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-17
      * Last Modified: 2021-10-23
      * Purpose: Process the PRINT command with parameter.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. print-text.

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

       01  ls-space-count                pic 9(10) comp value zero.
 
       01  ls-temp-variable-idx          pic 9(4) comp value 0.       
       
       01  ls-temp-param-buffer          pic x(1024).
       
       01  ls-temp-disp-num-val          pic z(15)9.

       linkage section.       

       01  l-src-code-str                pic x(1024). 

       01  l-screen-position.
           05  l-scr-row                 pic 999 value 1.
           05  l-scr-col                 pic 999 value 1.    

       01  l-text-colors.
           05  l-text-fg-color           pic 99 value 7.
           05  l-text-bg-color           pic 99 value 0.
           05  l-text-fg-highlight-sw    pic a value 'N'.
               88  l-text-fg-highlight   value 'Y'.
               88  l-text-fg-lowlight    value 'N'.

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
           l-src-code-str l-screen-position l-text-colors
           l-variable-table.   

       main-procedure.

      *>   If string still has leading "PRINT" command, take it out.
           if upper-case(l-src-code-str(1:length(ws-print))) = ws-print
           then 
               move trim(l-src-code-str(length(ws-print) + 1:))
                   to ls-temp-param-buffer
           else 
               move trim(l-src-code-str) to ls-temp-param-buffer
           end-if 

           *> check to see if printing variable value.
           if ls-temp-param-buffer(1:1) not = '"' then 
               
               perform varying ls-temp-variable-idx from 1 by 1 
               until ls-temp-variable-idx > l-num-variables

                   if upper-case(ls-temp-param-buffer)
                   = l-variable-name(ls-temp-variable-idx) then 
                   
      *>   If variable value is a number, remove leading zeros before 
      *>   moving it to the temp param buffer.
                       if l-type-integer(ls-temp-variable-idx) then 
                           move 
                           l-variable-value-num(ls-temp-variable-idx)
                           to ls-temp-disp-num-val

                           move ls-temp-disp-num-val
                           to ls-temp-param-buffer
                       else 
                           move l-variable-value(ls-temp-variable-idx)
                               to ls-temp-param-buffer
                       end-if 
                       exit perform 
                   end-if 
               end-perform 

           else 
           *> Replace first and last '"' from string.
               inspect ls-temp-param-buffer 
                   replacing first '"' by space 
                   
               inspect reverse(ls-temp-param-buffer)  
                   tallying ls-space-count for leading spaces
                   
               move spaces to ls-temp-param-buffer(
                   length(ls-temp-param-buffer) - ls-space-count:)             
           end-if 

           if l-text-fg-highlight then 
               display trim(ls-temp-param-buffer)
                 at l-screen-position
                   highlight
                   foreground-color l-text-fg-color  
                   background-color l-text-bg-color       
               end-display
           else 
               display trim(ls-temp-param-buffer)
                   at l-screen-position
                   foreground-color l-text-fg-color 
                   background-color l-text-bg-color        
               end-display
           end-if 

           call "logger" using concatenate(
               "PRINT :: location: " l-screen-position
               " colors: " l-text-colors
               " text: " trim(ls-temp-param-buffer))
           end-call 
                 
           add 1 to l-scr-row
           move 1 to l-scr-col                   

           goback.

       end program print-text.
