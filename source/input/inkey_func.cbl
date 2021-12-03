      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-08
      * Last Modified: 2021-12-01
      * Purpose: Process INKEY$ - get kb input.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       function-id. inkey-func.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.  
           crt status is ws-crt-status.         

       input-output section.
           
       data division.

       working-storage section.      

       copy screenio.

       01  ws-crt-status.
           05  ws-crt-status-key-1     pic 99.
           05  ws-crt-status-key-2     pic 99.


       local-storage section.
       
       01  ls-user-key-in              pic x.

       01  ls-working-key-in           pic xx.

       01  ls-use-trim-sw              pic a value 'Y'.
           88  ls-use-trim             value 'Y'.
           88  ls-not-use-trim         value 'N'.


       linkage section.       

       01  l-user-input                pic x(4). 

       procedure division returning l-user-input.
           set environment "COB_SCREEN_EXCEPTIONS" to 'Y'.
           set environment "COB_SCREEN_ESC" to 'Y'.
           set environment "COB_TIMEOUT_SCALE" to '3'.
              
       main-procedure.                

           move spaces to l-user-input

      *>   Hide cursor until it's unhidden again.
           call static "curs_set" using by value 0 
          
           accept ls-user-key-in    
               at 0181            
               with 
               auto-skip no-echo 
               timeout after 5 
           end-accept     

           move ls-user-key-in to ls-working-key-in

      *> Do additional transformation here for arrow keys and function 
      *> keys                
           evaluate ws-crt-status 

               when COB-SCR-KEY-DOWN
                   move " P" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-KEY-UP
                   move " H" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-KEY-LEFT
                   move " K" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-KEY-RIGHT
                   move " M" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F1
                   move " ;" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F2
                   move " <" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F3 
                   move " =" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F4
                   move " >" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F5
                   move " ?" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F6
                   move " @" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F7
                   move " A" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F8
                   move " B" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F9
                   move " C" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-F10
                   move " D" to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-TAB 
                   move "9 " to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-BACKSPACE
                   move "8 " to ls-working-key-in
                   set ls-not-use-trim to true 

               when COB-SCR-ESC
                   move "27" to ls-working-key-in

      *>       TODO : Code more special keys.

           end-evaluate

      *>   Check if actual space bar pressed or just a time out. For now
      *>   using ASCII char code as placeholder on timeout.
      *>   TODO : Figure out a way to differentiate between SPACE and ENTER
           if ls-working-key-in = space then 
               if ws-crt-status = COB-SCR-TIME-OUT then 
                   move '"32"' to l-user-input 
               else 
                   move '" "' to l-user-input
               end-if                    
           else 
               *> Don't use trim on special keys.
               if ls-not-use-trim then 
                   string '"' ls-working-key-in '"' into l-user-input
               else 
                   string 
                       '"'
                       trim(ls-working-key-in) 
                       '"'
                       into l-user-input
                   end-string 
               end-if 
           end-if 
               

           goback.
           

       end function inkey-func.
