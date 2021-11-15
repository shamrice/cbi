      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-20
      * Last Modified: 2021-11-15
      * Purpose: Checks if value passed is a BASIC keyword or not.
      *          Returns true(1) if it is or false(0) if it is not.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. is-keyword.

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

       01  ls-new-var-name-temp          pic x(1024).

       01  ls-keyword-found-count        pic 9(4) comp.
       01  ls-keyword-found-count-disp   pic 9(4).
   
       linkage section.       

       01  l-new-var-name                pic x(1024).        

       01  l-return-code                 pic 9 value 0.
           88  l-return-code-false       value 0.
           88  l-return-code-true        value 1.


       procedure division using 
           l-new-var-name l-return-code.   

       main-procedure.

           set l-return-code-false to true 

           move upper-case(l-new-var-name) to ls-new-var-name-temp

           inspect ls-new-var-name-temp
           replacing 
               all ws-comment-rem by spaces 
               all ws-cls by spaces 
               all ws-color by spaces 
               all ws-print by spaces 
               all ws-locate by spaces 
               all ws-end by spaces 
               all ws-system by spaces 
               all ws-stop by spaces 
               all ws-input by spaces 
               all ws-do by spaces 
               all ws-loop by spaces  
               all ws-while by spaces 
               all ws-wend by spaces  
               all ws-for by spaces  
               all ws-next by spaces  
               all ws-sub by spaces 
               all ws-call by spaces 
               all ws-goto by spaces 
               all ws-gosub by spaces 
               all ws-select-case by spaces 
               all ws-case by spaces 
               all ws-end-select by spaces  
               all ws-declare by spaces  
               all ws-return by spaces  
               all ws-screen by spaces  
               all ws-width by spaces  
               all ws-line by spaces  
               all ws-circle by spaces  
               all ws-sound by spaces  
               all ws-play by spaces  
               all ws-beep by spaces 
               all ws-if by spaces  
               all ws-then by spaces  
               all ws-elseif by spaces  
               all ws-else by spaces  
               all ws-end-if by spaces                 
               all ws-dim-shared by spaces 
               all ws-on-error by spaces  
               all ws-open by spaces  
               all ws-close by spaces
               all ws-pset by spaces 
               all ws-paint by spaces 
               all ws-put by spaces 
               all ws-draw by spaces 
               all ws-defint by spaces
               all ws-inkey by spaces 
               all ws-chr by spaces  
               
           if ls-new-var-name-temp = spaces then 
               set l-return-code-true to true 
           end-if 

           call "logger" using concatenate(
               "IS-KEYWORD :: Variable name checked: " 
               upper-case(trim(l-new-var-name))               
               " : returning: " l-return-code)
           end-call 

           goback.

       end program is-keyword.
