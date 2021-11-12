      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-11
      * Last Modified: 2021-11-11
      * Purpose: Paints background with passed background color.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. paint-background.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".
      
       01  ws-scr-lines         usage binary-char unsigned.
       01  ws-scr-cols          usage binary-char unsigned.

       01  ws-scr-lines-disp    pic 99.
       01  ws-scr-cols-disp     pic 99.

  
       local-storage section.       

       01  ls-paint-scr-string           pic x(1024).

       01  ls-paint-screen-position.
           05  ls-scr-row                pic 999 value 1.
           05  ls-scr-col                pic 999 value 1.         

       linkage section.       

       01  l-text-colors.
           05  l-text-fg-color           pic 99 value 7.
           05  l-text-bg-color           pic 99 value 0.
           05  l-text-fg-highlight-sw    pic a value 'N'.
               88  l-text-fg-highlight   value 'Y'.
               88  l-text-fg-lowlight    value 'N'.
           
       copy "copybooks/linkage_section/l_variable_table.cpy".
              

       procedure division using 
           l-text-colors l-variable-table.   

       main-procedure.
           
           call "logger" using "paiting background..."           

           call "CBL_GET_SCR_SIZE" using ws-scr-lines, ws-scr-cols

           move ws-scr-lines to ws-scr-lines-disp
           move ws-scr-cols to ws-scr-cols-disp

           string ws-print '"'
               into ls-paint-scr-string
           
           move '"' 
               to ls-paint-scr-string(ws-scr-cols + length(ws-print):1)
                      
           
           call "logger" using concatenate(
               "scr-lines: " ws-scr-lines 
               "scr-cols: " ws-scr-cols 
               "print statement: " ls-paint-scr-string
           ) end-call 

           move 1 to ls-scr-row
           move 1 to ls-scr-col
           perform ws-scr-lines times                               

               call "print-text" using 
                   ls-paint-scr-string
                   ls-paint-screen-position
                   l-text-colors
                   l-variable-table
               end-call 

           end-perform 


           goback.

       end program paint-background.
