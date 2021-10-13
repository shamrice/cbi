      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-10-12
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

       local-storage section.
       
       01  ls-var-idx                   pic 9(4) comp value 0.

       01  ls-comma-count               pic 9(10) comp value zero.
      
       01  ls-temp-param-buffer         pic x(1024).
       01  ls-temp-param-values         pic x(1024) occurs 10 times.          

       linkage section.       

       01  l-src-code-str                pic x(1024). 

       01  l-text-colors.
           05  l-text-fg-color           pic 99 value 7.
           05  l-text-bg-color           pic 99 value 0.
           05  l-text-fg-highlight-sw    pic a value 'N'.
               88  l-text-fg-highlight  value 'Y'.
               88  l-text-fg-lowlight   value 'N'.
           

       procedure division using l-src-code-str l-text-colors.   

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
               move ls-temp-param-values(1) to l-text-fg-color
               if l-text-fg-color > 7 then 
                   set l-text-fg-highlight to true 
                   subtract 8 from l-text-fg-color 
               else 
                   set l-text-fg-lowlight to true 
               end-if 
           end-if 

           if ls-temp-param-values(2) not = spaces then 
               move ls-temp-param-values(2) to l-text-bg-color
               if l-text-bg-color > 7 then                            
                   subtract 8 from l-text-bg-color                        
               end-if 
           end-if                        

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

       end program set-cursor-color.
