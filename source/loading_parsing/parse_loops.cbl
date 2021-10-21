      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-20
      * Last Modified: 2021-10-21
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

       01  ws-nested-idx             pic 9(10) comp.

       local-storage section.
       
       01  ls-cur-line-num-disp      pic 9(10).
       01  ls-num-loops-disp         pic 9(10).
       01  ls-nested-idx-disp        pic 9(10).

       linkage section.       

       01  l-src-code-str            pic x(1024). 

       01  l-cur-line-num            pic 9(10) comp.

       01  l-loop-boundary-table.
           05  l-num-loops           pic 9(10) comp value 0. 
           05  l-loop-data           occurs 0 to unbounded times
                                     depending on l-num-loops.               
               10  l-loop-start      pic 9(10). *>TODO Make comp 
               10  l-loop-end        pic 9(10).

       procedure division using 
           l-src-code-str l-cur-line-num l-loop-boundary-table.   

       main-procedure.

      *>   Check if line is a loop, if so add to loop table.
           if upper-case(l-src-code-str(1:length(ws-while)))
               = ws-while 
           then 
               add 1 to l-num-loops
               add 1 to ws-nested-idx
               move l-cur-line-num to l-loop-start(ws-nested-idx) 

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-loops to ls-num-loops-disp
               move ws-nested-idx to ls-nested-idx-disp
               call "logger" using concatenate(
                   "LOAD :: found WHILE-WEND loop START at: " 
                   ls-cur-line-num-disp
                   " : number of loops: " ls-num-loops-disp
                   " : nested level: " ls-nested-idx-disp)
               end-call 
           end-if 

           if upper-case(l-src-code-str(1:length(ws-wend)))
               = ws-wend 
           then                               
               move l-cur-line-num to l-loop-end(ws-nested-idx) 

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-loops to ls-num-loops-disp
               move ws-nested-idx to ls-nested-idx-disp
               call "logger" using concatenate(
                   "LOAD :: found WHILE-WEND loop END at: " 
                   ls-cur-line-num-disp
                   " : number of loops: " ls-num-loops-disp
                   " : nested level: " ls-nested-idx-disp)                   
               end-call 

               subtract 1 from ws-nested-idx
           end-if 

           goback.

       end program parse-loops.
