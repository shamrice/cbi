      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-20
      * Last Modified: 2021-11-05
      * Purpose: During loading, populates loop table with start and end
      *          line locations.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. parse-loops.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.

       copy "copybooks/basic_keywords.cpy".

       01  ws-nested-idx             pic 9(4) comp value 0.

       01  ws-nested-loop-idx        pic 9(4)  
                                     occurs 0 to 9999 times 
                                     depending on ws-nested-idx.

       local-storage section.
       
       01  ls-cur-line-num-disp      pic 9(10).
       01  ls-num-loops-disp         pic 9(4).
       01  ls-nested-idx-disp        pic 9(4).

       01  ls-end-loop-idx           pic 9(4).

       linkage section.              

       01  l-src-code-str            pic x(1024). 

       01  l-cur-line-num            pic 9(10) comp.
       
       copy "copybooks/linkage_section/l_loop_boundary_table.cpy".

       procedure division using 
           l-src-code-str l-cur-line-num l-loop-boundary-table.   

       main-procedure.

      *>   Check if line is a loop, if so add to loop table.
           if upper-case(l-src-code-str(1:length(ws-while)))
               = ws-while 
              or upper-case(l-src-code-str(1:length(ws-do))) = ws-do   
              or upper-case(l-src-code-str(1:length(ws-for))) = ws-for
           then 
               add 1 to l-num-loops
               add 1 to ws-nested-idx

      *>   Keep track of loop idx and nest level relationship.
               move l-num-loops to ws-nested-loop-idx(ws-nested-idx)
      
               move l-cur-line-num to l-loop-start(l-num-loops) 

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-loops to ls-num-loops-disp
               move ws-nested-idx to ls-nested-idx-disp
               call "logger" using concatenate(
                   "LOAD :: found loop START at: " 
                   ls-cur-line-num-disp
                   " : number of loops: " ls-num-loops-disp
                   " : nested level: " ls-nested-idx-disp
                   " : nested loop idx: " 
                   ws-nested-loop-idx(ws-nested-idx))
               end-call 
           end-if 

           if ws-nested-idx > 0 and (
               upper-case(l-src-code-str(1:length(ws-wend)))
               = ws-wend 
               or upper-case(l-src-code-str(1:length(ws-loop))) 
               = ws-loop  
               or upper-case(l-src-code-str(1:length(ws-next))) 
               = ws-next)
           then                               
      
      *>      Get related start/end loop index based on current nest level.
               move ws-nested-loop-idx(ws-nested-idx) 
                   to ls-end-loop-idx               

               move l-cur-line-num 
                   to l-loop-end(ls-end-loop-idx)

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-loops to ls-num-loops-disp
               move ws-nested-idx to ls-nested-idx-disp
               call "logger" using concatenate(
                   "LOAD :: found loop END at: " 
                   ls-cur-line-num-disp
                   " : for loop idx: " ls-end-loop-idx
                   " : related start line: " 
                       l-loop-start(ls-end-loop-idx)
                   " : number of loops: " ls-num-loops-disp
                   " : nested level: " ls-nested-idx-disp)                   
               end-call 

               subtract 1 from ws-nested-idx               
           end-if 

           goback.

       end program parse-loops.
