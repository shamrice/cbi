      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-25
      * Last Modified: 2021-10-27
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
       01  ls-sub-idx                pic 9(4) comp. 

       01  ls-cur-line-num-disp      pic 9(10).

       linkage section.       

       01  l-src-code-str            pic x(1024). 
 
       01  l-cur-line-num            pic 9(10) comp.

       01  l-sub-boundary-table.
           05  l-num-subs            pic 9(4) comp. 
           05  l-sub-data            occurs 0 to 1000 times
                                     depending on l-num-subs.   
               10  l-sub-name        pic x(32).                                                 
               10  l-sub-start       pic 9(10). *>TODO Make comp 
               10  l-sub-end         pic 9(10).  
               10  l-sub-cur-nest    pic 9(4) value 0.
               10  l-sub-last-call   pic 9(10) occurs 1000 times.
                                     *>idx of last call is cur nest. 

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

           perform varying ls-sub-idx from 1 by 1 
           until ls-sub-idx > l-num-subs 
               
               if l-sub-name(ls-sub-idx) = ls-temp-sub-name then 

               *> Add to nest idx (invoke count) and keep track of this
               *> as source called line. Then redirect processing to sub
                   add 1 to l-sub-cur-nest(ls-sub-idx)
                   
                   move l-cur-line-num 
                   to l-sub-last-call(
                       ls-sub-idx, 
                       l-sub-cur-nest(ls-sub-idx))

                   *>move l-sub-start(ls-sub-idx) to l-cur-line-num 
                   compute l-cur-line-num = l-sub-start(ls-sub-idx) - 1

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
