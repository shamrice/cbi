      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-08
      * Last Modified: 2021-11-08
      * Purpose: Process the GOSUB and GOTO commands with parameter.
      *          Commands are the same except GOSUB has a return line
      *          number whereas GOTO is just a single jump.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. gosub-goto-cmd.

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
    
       01  ls-temp-label-name        pic x(32).    
       01  ls-label-idx              pic 9(4) comp. 

       01  ls-cur-line-num-disp      pic 9(5).

       01  ls-command-type-sw        pic a(5).
           88  ls-command-type-error value "ERROR".
           88  ls-command-type-goto  value "GOTO".
           88  ls-command-type-gosub value "GOSUB".

       linkage section.       

       01  l-src-code-str            pic x(1024). 
 
       01  l-cur-line-num            pic 9(5) comp.

       copy "copybooks/linkage_section/l_line_label_boundary_table.cpy".


       procedure division using 
           l-src-code-str l-cur-line-num l-line-label-boundary-table.   

       main-procedure.

           if l-num-line-labels = 0 then 
               call "logger" using concatenate(
                  "GOSUB-GOTO-CMD :: Call without any labels declared. "
                  "Ignoring.")
               end-call 
               goback 
           end-if 

           call "logger" using concatenate(
               "GOSUB-GOTO-CMD :: Entered with string: "
               trim(l-src-code-str))
           end-call 

      *>   Determine if it's a goto or go sub call.
           set ls-command-type-error to true

           if trim(upper-case(l-src-code-str(1:length(ws-goto))))
               = ws-goto 
           then 
               set ls-command-type-goto to true 
               move trim(upper-case(l-src-code-str(length(ws-goto):)))
               to ls-temp-label-name
           end-if 

           if trim(upper-case(l-src-code-str(1:length(ws-gosub))))
               = ws-gosub 
           then 
               set ls-command-type-gosub to true 
               move trim(upper-case(l-src-code-str(length(ws-gosub):)))
               to ls-temp-label-name
           end-if 

           if ls-command-type-error then 
               call "logger" using 
                   "GOSUB-GOTO-CMD :: Not a valid GOSUB/GOTO. Skipping"
               end-call 
               goback 
           end-if            
           
      *>   Make sure there's actually a label to be called.
           if ls-temp-label-name = spaces then 
               call "logger" using concatenate(
                   "GOSUB-GOTO-CMD :: "
                   ls-command-type-sw    
                   " statement without values. "
                   "Skipping"
                   trim(l-src-code-str))
               end-call
               goback                
           end-if               

           call "logger" using ls-temp-label-name

           perform varying ls-label-idx from 1 by 1 
           until ls-label-idx > l-num-line-labels 
               
               if l-label-name(ls-label-idx) = ls-temp-label-name then 

               *> keep track of this as source called line. Then 
               *> redirect processing to label                                                                            
                   if ls-command-type-gosub then 
                       move l-cur-line-num 
                       to l-label-last-call(ls-label-idx)
                   end-if 

                   move l-label-start(ls-label-idx) to l-cur-line-num                   

                   move l-cur-line-num to ls-cur-line-num-disp
                   call "logger" using concatenate(
                       "GOSUB-GOTO-CMD :: found "
                       ls-command-type-sw
                       " label: " 
                       trim(ls-temp-label-name)
                       " : moving line idx to: " ls-cur-line-num-disp)
                   end-call 
                   exit perform 
               end-if 
           end-perform           
           goback.

       end program gosub-goto-cmd.
