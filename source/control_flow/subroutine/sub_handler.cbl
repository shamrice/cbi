      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-25
      * Last Modified: 2021-11-05
      * Purpose: Handles SUBROUTINE directing control flow to proper 
      *          entry and exit processing if current line is SUB related.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. sub-handler.

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
    
       01  ls-temp-sub-name          pic x(32).    
       01  ls-sub-idx                pic 9(4) comp. 

       01  ls-cur-line-num-disp      pic 9(10).

       linkage section.       

       01  l-src-code-str            pic x(1024). 
 
       01  l-cur-line-num            pic 9(10) comp.

       copy "copybooks/linkage_section/l_sub_boundary_table.cpy". 


       procedure division using 
           l-src-code-str l-cur-line-num l-sub-boundary-table.   

       main-procedure.

           if l-num-subs = 0 then 
               call "logger" using concatenate(
                   "SUB-HANDLER :: No subs declared. Ignoring.")
               end-call 
               goback 
           end-if 

           evaluate true 
               when upper-case(l-src-code-str(1:length(ws-sub))) 
                   = ws-sub             
                   perform handle-sub-start

               when upper-case(l-src-code-str(1:length(ws-end-sub)))
                   = ws-end-sub 
                   perform handle-sub-end 

           end-evaluate

           
           goback.


       handle-sub-start.
           call "logger" using "SUB-HANDLER :: processing SUB start"
               
           perform varying ls-sub-idx from 1 by 1
           until ls-sub-idx > l-num-subs 
               if l-sub-start(ls-sub-idx) = l-cur-line-num then 

                   if l-sub-cur-nest(ls-sub-idx) = 0 then 
                       move l-cur-line-num to ls-cur-line-num-disp
                       call "logger" using concatenate(
                           "SUB-HANDLER :: SUBROUTINE start on line " 
                            ls-cur-line-num-disp
                           " has not been invoked yet. Skipping "
                           " to END SUB at line "
                           l-sub-end(ls-sub-idx)) 
                       end-call 

                   *> -1 because program counter will add line at next loop
                       compute l-cur-line-num =
                            l-sub-end(ls-sub-idx) - 1    
                       end-compute 
                           
                       exit perform                        
                  end-if 

           end-perform           
           exit paragraph.



       handle-sub-end.

           call "logger" using "SUB-HANDLER :: processing SUB END"

      *>   Iterate through loop table and find last called line of
      *>   sub and subtract the nest index by 1.
           perform varying ls-sub-idx from 1 by 1
           until ls-sub-idx > l-num-subs 
               
               if l-sub-end(ls-sub-idx) = l-cur-line-num then 
                   
                   
                   if l-sub-cur-nest(ls-sub-idx) > 0 then 
                       move l-sub-last-call(
                           ls-sub-idx, l-sub-cur-nest(ls-sub-idx))
                           to l-cur-line-num 

                       subtract 1 from l-sub-cur-nest(ls-sub-idx) 
                       
                       move l-cur-line-num to ls-cur-line-num-disp
                       call "logger" using concatenate(
                           "SUB-HANDLER :: found END SUB. Redirecting "
                           "to last line to call sub: " 
                           ls-cur-line-num-disp)
                       end-call 
                   else 
                       call "logger" using concatenate(
                           "SUB-HANDLER :: found END SUB. Current SUB "
                           "was not invoked, so ignoring and moving "
                           "to next line in the program.")
                       end-call                    
                   end-if                    
                   
                   exit perform 
               end-if 

           end-perform 

           exit paragraph.

       end program sub-handler.
