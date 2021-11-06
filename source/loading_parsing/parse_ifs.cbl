      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-05
      * Last Modified: 2021-11-05
      * Purpose: During loading, populates if table 
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. parse-ifs.

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

       01  ws-nested-if-idx          pic 9(4)  
                                     occurs 0 to 9999 times 
                                     depending on ws-nested-idx.       

       local-storage section.
       
       01  ls-cur-line-num-disp      pic 9(5).
       01  ls-num-ifs-disp           pic 9(4).
       01  ls-num-elseifs-disp       pic 99.
       01  ls-nested-idx-disp        pic 9(4).

       01  ls-end-if-idx             pic 9(4).
       01  ls-else-idx               pic 9(4).
       01  ls-elseif-idx             pic 9(4).

       linkage section.       

       01  l-src-code-str            pic x(1024). 

       01  l-cur-line-num            pic 9(5) comp.

       copy "copybooks/linkage_section/l_if_boundary_table.cpy".

       procedure division using 
           l-src-code-str l-cur-line-num l-if-boundary-table.   

       main-procedure.

           call "logger" using concatenate(
               "LOAD:PARSE-IFS :: Enter PARSE-IFS with line: "
               trim(l-src-code-str))
           end-call 
      
      *>   Check if line is start of IF, if so add to loop table.
           if upper-case(l-src-code-str(1:length(ws-if)))
               = ws-if               
           then 
               add 1 to l-num-ifs
               add 1 to ws-nested-idx

      *>   Keep track of if idx and nest level relationship.
               move l-num-ifs to ws-nested-if-idx(ws-nested-idx)
      
               move l-cur-line-num to l-if-start(l-num-ifs) 
               set l-if-not-processed(l-num-ifs) to true 

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-ifs to ls-num-ifs-disp
               move ws-nested-idx to ls-nested-idx-disp
               call "logger" using concatenate(
                   "LOAD:PARSE-IFS :: found IF START at: " 
                   ls-cur-line-num-disp
                   " : number of IFs: " ls-num-ifs-disp
                   " : nested level: " ls-nested-idx-disp
                   " : nested loop idx: " 
                   ws-nested-if-idx(ws-nested-idx))
               end-call 
               goback 
           end-if 


           if ws-nested-idx > 0 and 
               upper-case(l-src-code-str(1:length(ws-elseif)))
               = ws-elseif 
           then 
               move ws-nested-if-idx(ws-nested-idx) 
                   to ls-elseif-idx  

               add 1 to l-num-elseifs(ls-elseif-idx) 

               move l-cur-line-num 
               to l-elseif-start
                   (ls-elseif-idx,
                   l-num-elseifs(ls-elseif-idx))

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-ifs to ls-num-ifs-disp
               move ws-nested-idx to ls-nested-idx-disp
               move l-num-elseifs(ls-elseif-idx) to ls-num-elseifs-disp
               call "logger" using concatenate(
                   "LOAD:PARSE-IFS :: found ELSEIF at: " 
                   ls-cur-line-num-disp
                   " : IF idx: " ls-elseif-idx
                   " : related start line: " 
                       l-if-start(ls-elseif-idx)
                   " : number of IFs: " ls-num-ifs-disp
                   " : number of ELSEIFs in IF: "
                       ls-num-elseifs-disp
                   " : nested level: " ls-nested-idx-disp)                   
               end-call                
               goback 
           end-if 


           if ws-nested-idx > 0 and 
               upper-case(l-src-code-str(1:length(ws-else)))
               = ws-else                               
           then                                     
      *>      Get related start/end loop index based on current nest level.
               move ws-nested-if-idx(ws-nested-idx) 
                   to ls-else-idx               

               move l-cur-line-num 
                   to l-else-start(ls-else-idx)

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-ifs to ls-num-ifs-disp
               move ws-nested-idx to ls-nested-idx-disp
               call "logger" using concatenate(
                   "LOAD:PARSE-IFS :: found ELSE at: " 
                   ls-cur-line-num-disp
                   " : IF idx: " ls-else-idx
                   " : related start line: " 
                       l-if-start(ls-else-idx)
                   " : number of IFs: " ls-num-ifs-disp
                   " : nested level: " ls-nested-idx-disp)                   
               end-call 
               goback                
           end-if 

           if ws-nested-idx > 0 and 
               upper-case(l-src-code-str(1:length(ws-end-if)))
               = ws-end-if                               
           then                                     
      *>      Get related start/end loop index based on current nest level.
               move ws-nested-if-idx(ws-nested-idx) 
                   to ls-end-if-idx               

               move l-cur-line-num 
                   to l-if-end(ls-end-if-idx)

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-ifs to ls-num-ifs-disp
               move ws-nested-idx to ls-nested-idx-disp
               call "logger" using concatenate(
                   "LOAD:PARSE-IFS :: found END IF at: " 
                   ls-cur-line-num-disp
                   " : IF idx: " ls-end-if-idx
                   " : related start line: " 
                       l-if-start(ls-end-if-idx)
                   " : number of IFs: " ls-num-ifs-disp
                   " : nested level: " ls-nested-idx-disp)                   
               end-call 

               subtract 1 from ws-nested-idx               
           end-if 

           goback.

       end program parse-ifs.
