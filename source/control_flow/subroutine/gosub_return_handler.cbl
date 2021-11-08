      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-08
      * Last Modified: 2021-11-08
      * Purpose: Handles RETURN statements for GOSUBs
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. gosub-return-handler.

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

       linkage section.       

       01  l-src-code-str            pic x(1024). 
 
       01  l-cur-line-num            pic 9(5) comp.

       copy "copybooks/linkage_section/l_line_label_boundary_table.cpy". 


       procedure division using 
           l-src-code-str l-cur-line-num l-line-label-boundary-table.   

       main-procedure.

           if l-num-line-labels = 0 then 
               call "logger" using concatenate(
                   "LINE_LABEL-HANDLER :: No labels declared. Ignoring."
               )
               end-call 
               goback 
           end-if 

           if upper-case(l-src-code-str(1:length(ws-return))) 
               = ws-return             
           then 

               call "logger" using 
                   "LINE-LABEL-HANDLER :: GOSUB processing RETURN"
               end-call

      *>   Iterate through loop table and find last called line of
      *>   GOSUB. Only redirect if last call was initialized with a val.
               perform varying ls-label-idx from 1 by 1
               until ls-label-idx > l-num-line-labels 
               
                   if l-label-end(ls-label-idx) = l-cur-line-num 
                       and l-label-last-call(ls-label-idx) > 0 
                   then 
                   
                       move l-label-last-call(ls-label-idx)
                           to l-cur-line-num 

                       move l-cur-line-num to ls-cur-line-num-disp
                       call "logger" using concatenate(
                           "LINE-LABEL-HANDLER :: found RETURN. "
                           "Redirecting to last line to call GOSUB: " 
                           ls-cur-line-num-disp)
                       end-call
                       exit perform
                   end-if                                       
               end-perform 
           end-if 
           goback.

       end program gosub-return-handler.
