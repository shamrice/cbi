      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-17
      * Last Modified: 2021-11-22
      * Purpose: Process the PRINT command.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. print-text.

       environment division.
       
       configuration section.

       repository. 
           function ascii-code-to-char
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       78  ws-print-var-separator       value "; ".

       local-storage section.       
       
       copy "copybooks/local_storage/ls_variable.cpy".  

       01  ls-trailing-space-count       pic 9(4) comp.                    

       01  ls-str-pointer                pic 9(4) comp.
       01  ls-last-char-idx              pic 9(4) comp value 1.

       01  ls-temp-variable-idx          pic 9(4) comp value 0.       
       
       01  ls-length-of-str              pic 9(4) comp.       

       01  ls-temp-str-buffer            pic x(1024).
       
       01  ls-output-buffer              pic x(1024).
       
       01  ls-temp-disp-num-val          pic -(16)9.       

    
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

      
       procedure division using 
           l-src-code-str l-screen-position l-text-colors.   

       main-procedure.

           move 1 to ls-str-pointer

           *> break print statement into text chunks and variable chunks
           perform until ls-str-pointer > length(l-src-code-str) 
               
               unstring l-src-code-str 
                   delimited by ws-print-var-separator
                   into ls-temp-str-buffer                   
                   with pointer ls-str-pointer
               end-unstring

               *> remove leading "PRINT" command if exists.
               if upper-case(ls-temp-str-buffer(1:length(ws-print)))
                   = ws-print
               then 
                   move ls-temp-str-buffer(length(ws-print) + 1:)
                   to ls-temp-str-buffer
               end-if 

               *> Remove leading double quote if exists.
               if ls-temp-str-buffer(1:1) = '"' then 
                   move ls-temp-str-buffer(2:)
                   to ls-temp-str-buffer
               else 
                   *>If not, assume variable value substitution
                   perform set-variable-value
               end-if 
               
               *>Calculate number of spaces until trailing double quote
               move zero to ls-trailing-space-count
               inspect reverse(ls-temp-str-buffer)  
               tallying ls-trailing-space-count for leading spaces

               *>length of string is start to this value.
               compute ls-length-of-str = 
                   length(ls-temp-str-buffer) - 
                   ls-trailing-space-count
               end-compute 

               *> Remove trailing double quote if exists.
               if ls-trailing-space-count 
                   not = length(ls-temp-str-buffer)
                   and 
                   ls-temp-str-buffer(
                   ls-length-of-str:1) = '"' 
               then 
                   subtract 1 from ls-length-of-str
               end-if 
               
               *>Append string to output buffer based on last char offset
               move ls-temp-str-buffer(1:ls-length-of-str) 
               to ls-output-buffer(ls-last-char-idx:ls-length-of-str)

               add ls-length-of-str to ls-last-char-idx                               
              
           end-perform 

           subtract 1 from ls-last-char-idx 
                      
           if l-text-fg-highlight then       
               display ls-output-buffer(1:ls-last-char-idx)
                   at l-screen-position
                   highlight
                   foreground-color l-text-fg-color  
                   background-color l-text-bg-color       
               end-display
           else 
               display ls-output-buffer(1:ls-last-char-idx)
                   at l-screen-position
                   foreground-color l-text-fg-color 
                   background-color l-text-bg-color        
               end-display
           end-if 

           call "logger" using concatenate(
               "PRINT-TEXT :: location: " l-screen-position
               " colors: " l-text-colors
               " text: " trim(ls-output-buffer))
           end-call 
                 
      *>   PRINT command moves cursor to col 1 of next row.
           add 1 to l-scr-row
           move 1 to l-scr-col                   

           goback.



       set-variable-value.

      *>   Check for CHR$() function.
           if upper-case(trim(ls-temp-str-buffer(1:length(ws-chr))))
               = ws-chr 
           then 
               move ascii-code-to-char(ls-temp-str-buffer) 
               to ls-temp-str-buffer
               exit paragraph
           end-if 

      *>   Check if array and sub vars for index as eneded.
           call "array-indexed-name" using ls-temp-str-buffer

           move ls-temp-str-buffer to ls-variable-name  
           call "get-variable" using                
               ls-variable 
               ls-get-variable-return-code               
           end-call 

           if ls-get-variable-return-code = 0 then 
               exit paragraph
           end-if 

      *>   If variable value is a number, remove leading zeros before 
      *>   moving it to the temp param buffer.
           if ls-type-integer then                
               move ls-variable-value-num
               to ls-temp-disp-num-val

               move trim(ls-temp-disp-num-val)
               to ls-temp-str-buffer
           
           else 
      *>   String variables SHOULD be saved with quotes on them. If 
      *>   there, remove if from output string.
               if ls-variable-value(1:1) = '"' then 
                   move ls-variable-value(2:) to ls-temp-str-buffer 
               else 
                   call "logger" using concatenate(
                       "PRINT-TEXT :: WARNING : String variable: "
                       trim(ls-variable-name) " was not stored with "
                       "quotes. This may be due to an internal "
                       "assignment error or error in the source file. "
                       " : Value returned: "
                       ls-variable-value)
                   end-call 
                   move ls-variable-value to ls-temp-str-buffer
               end-if 
           end-if               
     
           exit paragraph.

       end program print-text.
