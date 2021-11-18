      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-11-05
      * Purpose: Processed the LOCATE command and sets cursor position
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. set-cursor-position.

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
       
       01  ls-temp-param-buffer         pic x(1024).
       01  ls-temp-param-values         pic x(16) occurs 2 times. 

       01  ls-variable-temp-data.
           05  ls-var-name               pic x(16).
           05  ls-var-type               pic x(8).
           05  ls-var-value              pic x(1024).
           05  ls-var-value-num          pic 9(16).
           05  ls-var-ret-code           pic 9.


       linkage section.       

       01  l-src-code-str               pic x(1024). 

       01  l-screen-position.
           05  l-scr-row                pic 999.
           05  l-scr-col                pic 999.    

       copy "copybooks/linkage_section/l_variable_table.cpy".
       

       procedure division using 
           l-src-code-str l-screen-position l-variable-table.   

       main-procedure.
           
      *>   Remove LOCATE command if exists.
           if upper-case(l-src-code-str(1:length(ws-locate))) 
               = ws-locate 
           then 
               move trim(l-src-code-str(length(ws-locate):)) 
               to ls-temp-param-buffer
           else
               move trim(l-src-code-str) to ls-temp-param-buffer
           end-if 
                         
           unstring ls-temp-param-buffer
               delimited by "," 
               into 
                   ls-temp-param-values(1) 
                   ls-temp-param-values(2)
           end-unstring

      *>   Either set the numeric value or get value from variable.
           if trim(ls-temp-param-values(1)) is numeric then 
               move ls-temp-param-values(1) to l-scr-row
           else 
               move upper-case(trim(ls-temp-param-values(1))) 
                   to ls-var-name
               perform set-value-from-var
               move ls-var-value to l-scr-row 
           end-if 

           if ls-temp-param-values(2) not = spaces then 
               if trim(ls-temp-param-values(2)) is numeric then 
                   move ls-temp-param-values(2) to l-scr-col
               else 
                   move upper-case(trim(ls-temp-param-values(2)))
                       to ls-var-name
                   perform set-value-from-var
                   move ls-var-value to l-scr-col 
               end-if 
           end-if 

           call "logger" using concatenate(
               "LOCATE :: row: " l-scr-row
               " col: " l-scr-col)
           end-call 
              
           goback.


       set-value-from-var.
           call "get-var-value" using                
               ls-var-name
               ls-var-type 
               ls-var-value
               ls-var-ret-code
           end-call

           if ls-var-ret-code = 0 or ls-var-type not = "INTEGER" then 
               call "logger" using concatenate(
                   "LOCATE :: Failed to get value for variable: "
                   trim(ls-var-name) " : Defaulting to 1.")
               end-call 
               move 1 to ls-var-value
           end-if            

           exit paragraph.

       end program set-cursor-position.
