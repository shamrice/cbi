      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-14
      * Last Modified: 2021-10-21
      * Purpose: Process the INPUT command with parameter.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. input-cmd.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       78  ws-input-type-normal-delmiter           value '",'.
       78  ws-input-type-question-delimiter        value '";'.

       local-storage section.
       
       01  ls-temp-input-val            pic x(1024) value spaces.
       01  ls-assign-var-str            pic x(1024).              

       01  ls-temp-param-buffer         pic x(1024).
       01  ls-temp-param-values         pic x(1024) occurs 10 times.                  

       01  ls-temp-input-text           pic x(1024).

       01  ls-char-idx                  pic 9(4) comp.
       01  ls-input-str-end-idx         pic 9(4).

       01  ls-input-type-sw             pic a value 'N'.
           88  ls-input-type-normal     value 'N'.
           88  ls-input-type-question   value 'Q'.

       01  ls-quote-count               pic 9(4) comp.

       linkage section.       

       01  l-src-code-str               pic x(1024). 

       01  l-screen-position.
           05  l-scr-row                pic 999 value 1.
           05  l-scr-col                pic 999 value 1.    

       01  l-text-colors.
           05  l-text-fg-color          pic 99 value 7.
           05  l-text-bg-color          pic 99 value 0.
           05  l-text-fg-highlight-sw   pic a value 'N'.
               88  l-text-fg-highlight  value 'Y'.
               88  l-text-fg-lowlight   value 'N'.

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

           move l-src-code-str(length(ws-input) + 1:)
               to ls-temp-param-buffer
           
           if ls-temp-param-buffer = spaces then 
               call "logger" using concatenate(
                   "INPUT :: input statement without values. Skipping "
                   trim(l-src-code-str))
               end-call
               goback                
           end-if               

           inspect ls-temp-param-buffer 
           tallying ls-quote-count for all '"'
          
           if ls-quote-count > 0 then 
               perform handle-input-with-text
           else 
               perform handle-input-var-only
           end-if 

           call "logger" using concatenate(
               "INPUT :: "
               " type: " ls-input-type-sw
               " end idx: " ls-input-str-end-idx
               " input statement: " trim(l-src-code-str)
               " param buffer: " trim(ls-temp-param-buffer)
               " text: " trim(ls-temp-input-text)
               " variable: " trim(ls-temp-param-values(1)))
           end-call 
              
           goback.


       handle-input-with-text.

           perform varying ls-char-idx from 1 by 1 
           until ls-char-idx > length(ls-temp-param-buffer) 
               evaluate true 
                   when ls-temp-param-buffer(ls-char-idx:2) 
                       = ws-input-type-normal-delmiter
               
                       set ls-input-type-normal to true 
                       move ls-char-idx to ls-input-str-end-idx
                       exit perform 

                   when ls-temp-param-buffer(ls-char-idx:2) 
                       = ws-input-type-question-delimiter
               
                       set ls-input-type-question to true 
                       move ls-char-idx to ls-input-str-end-idx
                       exit perform 

               end-evaluate
           end-perform 

           if ls-input-str-end-idx = 0 then 
               call "logger" using concatenate( 
                   "INPUT :: ERROR : Input string is malformed. "
                   "Input: " trim(l-src-code-str))
               end-call 
               goback 
           end-if 
              
      *>   Remove leading and end quote from input text string.
           move ls-temp-param-buffer(1:ls-input-str-end-idx) 
           to ls-temp-input-text
               
           inspect ls-temp-input-text 
           replacing first '"' by space  

           move spaces to ls-temp-input-text(ls-input-str-end-idx:)

      *>   Get input destination variable name
               *> TODO : Unstring into chain of variables delimited by 
               *>        a comma. 
           move trim(ls-temp-param-buffer(ls-input-str-end-idx + 2:))
           to ls-temp-param-values(1) 


      *>   Append question mark if semicolon is separator
           if ls-input-type-question then 
               move "?" 
                   to ls-temp-input-text(ls-input-str-end-idx:1)
               add 2 to ls-input-str-end-idx
           end-if 

           call "print-text" using 
               ls-temp-input-text 
               l-screen-position
               l-text-colors
               l-variable-table
           end-call  

           perform display-and-accept-input

           exit paragraph.       



       handle-input-var-only.

           set ls-input-type-question to true            
           move "?" to ls-temp-input-text           
           move 5 to ls-input-str-end-idx           

           call "print-text" using 
               ls-temp-input-text 
               l-screen-position
               l-text-colors
               l-variable-table
           end-call  

           move trim(ls-temp-param-buffer) to ls-temp-param-values(1) 

           perform display-and-accept-input
           
           exit paragraph.



       display-and-accept-input.
      *>   TODO : improve this
      *>   Set location of cursor after input text and accept.
           subtract 1 from l-scr-row 
           compute l-scr-col = ls-input-str-end-idx - 1
               
           accept ls-temp-input-val at l-screen-position

           add 1 to l-scr-row 
           move 1 to l-scr-col 

           if trim(ls-temp-input-val) is numeric then 
               move concatenate(
                   upper-case(trim(ls-temp-param-values(1)))
                   ' = '
                   trim(ls-temp-input-val))
                   to ls-assign-var-str               
           else 

               move concatenate(
                   upper-case(trim(ls-temp-param-values(1)))
                   ' = "'
                   trim(ls-temp-input-val)
                   '"')
                   to ls-assign-var-str               
           end-if 
           call "assign-var" using 
               ls-assign-var-str
               l-variable-table
           end-call 

           exit paragraph.

       end program input-cmd.
