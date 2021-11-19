      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-11
      * Last Modified: 2021-11-19
      * Purpose: Clears screen. Also paints background if screen mode 
      *          is 0. 
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. clear-screen.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".
            
      * 01  ws-prev-background-color      pic 99.
  
       local-storage section.       
    

       linkage section.       

       01  l-screen-mode                 pic 99.

       01  l-screen-position.
           05  l-scr-row                 pic 999.
           05  l-scr-col                 pic 999.

       01  l-text-colors.
           05  l-text-fg-color           pic 99 value 7.
           05  l-text-bg-color           pic 99 value 0.
           05  l-text-fg-highlight-sw    pic a value 'N'.
               88  l-text-fg-highlight   value 'Y'.
               88  l-text-fg-lowlight    value 'N'.


       procedure division using 
           l-screen-mode l-screen-position
           l-text-colors.   

       main-procedure.
           
           call "logger" using concatenate(
               "CLS :: Screen mode: " l-screen-mode
               " : background color: " l-text-bg-color)
           end-call 
          
           display space blank screen 
           move 1 to l-scr-col
           move 1 to l-scr-row 

      *> CLS on SCREEN 0 causes the background to be painted.
      *> Modifying IF so that always paints current background.
      *     if l-text-bg-color not = ws-prev-background-color 
      *         and l-screen-mode = 0 
           if l-screen-mode = 0 then 
               call "paint-background" using 
                   l-text-colors                   
               end-call
           end-if     

      *     move l-text-bg-color to ws-prev-background-color       

      *>   hide cursor by default on new screens.
           call static "curs_set" using by value 0 

           goback.

       end program clear-screen.
