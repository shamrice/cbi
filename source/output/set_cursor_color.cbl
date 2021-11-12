      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-11-11
      * Purpose: Processed the COLOR command and sets cursor color
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. set-cursor-color.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".
         
       01  ws-prev-bg-color     pic 99.

       
       local-storage section.       

       01  ls-comma-count                pic 9 comp value zero.
      
       01  ls-temp-param-buffer          pic x(1024).
       01  ls-temp-param-values          pic x(1024) occurs 2 times.  

       01  ls-variable-temp-data.
           05  ls-var-name               pic x(16).
           05  ls-var-type               pic x(8).
           05  ls-var-value              pic x(1024).
           05  ls-var-value-num          pic 9(16).
           05  ls-var-ret-code           pic 9.

       linkage section.       

       01  l-src-code-str                pic x(1024). 

       01  l-text-colors.
           05  l-text-fg-color           pic 99 value 7.
           05  l-text-bg-color           pic 99 value 0.
           05  l-text-fg-highlight-sw    pic a value 'N'.
               88  l-text-fg-highlight   value 'Y'.
               88  l-text-fg-lowlight    value 'N'.
           
       copy "copybooks/linkage_section/l_variable_table.cpy".

       01  l-screen-mode                 pic 99.
       

       procedure division using 
           l-src-code-str l-text-colors l-variable-table l-screen-mode.   

       main-procedure.

           move trim(l-src-code-str(6:)) to ls-temp-param-buffer           

           inspect ls-temp-param-buffer 
           tallying ls-comma-count for all "," 

           if ls-comma-count > 0 then 
               unstring ls-temp-param-buffer
                   delimited by "," into
                       ls-temp-param-values(1)
                       ls-temp-param-values(2)
               end-unstring
           else 
               move ls-temp-param-buffer to ls-temp-param-values(1)
               move spaces to ls-temp-param-values(2) 
           end-if 

           if ls-temp-param-values(1) not = spaces then
               if trim(ls-temp-param-values(1)) is numeric then 
                   move ls-temp-param-values(1) to l-text-fg-color
               else 
                   move ls-temp-param-values(1) to ls-var-name 
                   perform set-value-from-var
                   move ls-var-value to l-text-fg-color                    
               end-if 
               if l-text-fg-color > 7 then 
                   set l-text-fg-highlight to true 
                   subtract 8 from l-text-fg-color 
               else 
                   set l-text-fg-lowlight to true 
               end-if 
           end-if 

           if ls-temp-param-values(2) not = spaces then 
               if trim(ls-temp-param-values(2)) is numeric then 
                   move ls-temp-param-values(2) to l-text-bg-color
               else 
                   move trim(ls-temp-param-values(2)) to ls-var-name 
                   perform set-value-from-var
                   move ls-var-value to l-text-bg-color                    
               end-if                    
               if l-text-bg-color > 7 then                            
                   subtract 8 from l-text-bg-color                        
               end-if 
           end-if                        

      *> In screen mode 7 & 9, COLOR statement paints background.
      *> In screen mode 0 (default) CLS paints background.
           if l-text-bg-color not = ws-prev-bg-color 
               and (l-screen-mode = 7 or l-screen-mode = 9)
           then                
               call "paint-background" using 
                   l-text-colors
                   l-variable-table
               end-call 
           end-if

           move l-text-bg-color to ws-prev-bg-color 

           call "logger" using concatenate(
               "COLOR :: ws-text-fg-color: " l-text-fg-color 
               " ws-text-bg-color: " l-text-bg-color 
               " ws-temp-param-values(1): " 
               trim(ls-temp-param-values(1))
               " ws-temp-param-values(2): " 
               trim(ls-temp-param-values(2))
               " ws-temp-param-buffer: " trim(ls-temp-param-buffer))
           end-call 

           goback.




       set-value-from-var.           
           call "get-var-value" using 
               l-variable-table
               ls-var-name
               ls-var-type 
               ls-var-value
               ls-var-ret-code
           end-call

           if ls-var-ret-code = 0 or ls-var-type not = "INTEGER" then 
               call "logger" using concatenate(
                   "COLOR :: Failed to get value for variable: "
                   trim(ls-var-name) " : Defaulting to 0.")
               end-call 
               move 0 to ls-var-value
           end-if            

           exit paragraph.



       end program set-cursor-color.
