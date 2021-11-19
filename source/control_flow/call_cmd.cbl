      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-25
      * Last Modified: 2021-11-19
      * Purpose: Process the CALL command with parameter.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. call-cmd.

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
       01  ls-sub-end-idx            pic 9(4) comp. 

       01  ls-cur-line-num-disp      pic 9(5).

       linkage section.       

       01  l-src-code-str            pic x(1024). 
 
       01  l-cur-line-num            pic 9(5) comp.

       copy "copybooks/linkage_section/l_sub_boundary_table.cpy".


       procedure division using 
           l-src-code-str l-cur-line-num l-sub-boundary-table.   

       main-procedure.

           if l-num-subs = 0 then 
               call "logger" using concatenate(
                   "CALL :: CALL without any SUBs declared. Ignoring.")
               end-call 
               goback 
           end-if 

           call "logger" using concatenate(
               "CALL :: CALL entered with string: "
               trim(l-src-code-str))
           end-call 

           move trim(upper-case(l-src-code-str(length(ws-call):)))
               to ls-temp-sub-name
           
           if ls-temp-sub-name = spaces then 
               call "logger" using concatenate(
                   "CALL :: CALL statement without values. Skipping "
                   trim(l-src-code-str))
               end-call
               goback                
           end-if               

           call "logger" using ls-temp-sub-name

           set ls-sub-end-idx to l-num-subs 
           perform varying l-sub-idx from 1 by 1 
           until l-sub-idx > ls-sub-end-idx
               
               if l-sub-name(l-sub-idx) = ls-temp-sub-name then 

               *> Add to nest idx (invoke count) and keep track of this
               *> as source called line. Then redirect processing to sub
                   add 1 to l-sub-cur-nest(l-sub-idx)
                   
                   move l-cur-line-num 
                   to l-sub-last-call(
                       l-sub-idx, 
                       l-sub-cur-nest(l-sub-idx))

                   *>move l-sub-start(l-sub-idx) to l-cur-line-num 
                   compute l-cur-line-num = l-sub-start(l-sub-idx) - 1

                   move l-cur-line-num to ls-cur-line-num-disp
                   call "logger" using concatenate(
                       "CALL-CMD :: found sub: " 
                       trim(ls-temp-sub-name)
                       " : moving line idx to: " ls-cur-line-num-disp)
                   end-call 
                   exit perform 
               end-if 

           end-perform           
           goback.

       end program call-cmd.
