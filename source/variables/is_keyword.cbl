      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-20
      * Last Modified: 2021-11-05
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

           inspect upper-case(l-new-var-name)
           tallying ls-keyword-found-count for
               all ws-comment-rem
               all ws-cls 
               all ws-color 
               all ws-print 
               all ws-locate 
               all ws-end
               all ws-system
               all ws-stop
               all ws-input 
               all ws-do 
               all ws-loop 
               all ws-while 
               all ws-wend 
               all ws-for 
               all ws-next 
               all ws-sub     
               all ws-call
               all ws-goto  
               all ws-select-case 
               all ws-case
               all ws-end-select 
               all ws-declare 
               all ws-return 
               all ws-screen 
               all ws-width 
               all ws-line 
               all ws-circle 
               all ws-sound 
               all ws-play 
               all ws-if 
               all ws-elseif 
               all ws-end-if 
               all ws-const 
               all ws-dim-shared
               all ws-on-error 
               all ws-open 
               all ws-close 

               
           
           if ls-keyword-found-count > 0 then 
               set l-return-code-true to true 
           else 
               set l-return-code-false to true 
           end-if 

           move ls-keyword-found-count to ls-keyword-found-count-disp

           call "logger" using concatenate(
               "IS-KEYWORD :: Variable name checked: " 
               upper-case(trim(l-new-var-name))
               " : keyword count: " ls-keyword-found-count-disp
               " : returning: " l-return-code)
           end-call 

           goback.

       end program is-keyword.
