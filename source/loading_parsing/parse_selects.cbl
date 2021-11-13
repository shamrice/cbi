      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-13
      * Last Modified: 2021-11-13
      * Purpose: During loading, populates select table 
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. parse-selects.

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

       01  ws-nested-select-idx      pic 9(4)  
                                     occurs 0 to 9999 times 
                                     depending on ws-nested-idx.       

       local-storage section.
       
       01  ls-cur-line-num-disp      pic 9(5).
       01  ls-num-selects-disp       pic 9(4).
       01  ls-num-cases-disp         pic 99.
       01  ls-nested-idx-disp        pic 9(4).

       01  ls-end-select-idx         pic 9(4).
       01  ls-case-idx               pic 9(4).       

       linkage section.       

       01  l-src-code-str            pic x(1024). 

       01  l-cur-line-num            pic 9(5) comp.

       copy "copybooks/linkage_section/l_select_boundary_table.cpy".

       procedure division using 
           l-src-code-str l-cur-line-num l-select-boundary-table.   

       main-procedure.
      
      *>   Check if line is start of IF, if so add to loop table.
           if upper-case(l-src-code-str(1:length(ws-select-case)))
               = ws-select-case               
           then 
               add 1 to l-num-selects
               add 1 to ws-nested-idx

      *>   Keep track of if idx and nest level relationship.
               move l-num-selects to ws-nested-select-idx(ws-nested-idx)
      
               move l-cur-line-num to l-select-start(l-num-selects) 
               set l-select-not-processed(l-num-selects) to true 

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-selects to ls-num-selects-disp
               move ws-nested-idx to ls-nested-idx-disp
               call "logger" using concatenate(
                   "LOAD:PARSE-SELECTS :: found SELECT START at: " 
                   ls-cur-line-num-disp
                   " : number of SELECTs: " ls-num-selects-disp
                   " : nested level: " ls-nested-idx-disp
                   " : nested loop idx: " 
                   ws-nested-select-idx(ws-nested-idx)
                   " : Line: " trim(l-src-code-str))
               end-call 
               goback 
           end-if 


           if ws-nested-idx > 0 and 
               upper-case(l-src-code-str(1:length(ws-case)))
               = ws-case 
           then 
               move ws-nested-select-idx(ws-nested-idx) 
                   to ls-case-idx  

               add 1 to l-num-cases(ls-case-idx) 

               move l-cur-line-num 
               to l-case-start
                   (ls-case-idx,
                   l-num-cases(ls-case-idx))

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-selects to ls-num-selects-disp
               move ws-nested-idx to ls-nested-idx-disp
               move l-num-cases(ls-case-idx) to ls-num-cases-disp
               call "logger" using concatenate(
                   "LOAD:PARSE-SELECTS :: found CASE at: " 
                   ls-cur-line-num-disp
                   " : CASE idx: " ls-case-idx
                   " : related start line: " 
                       l-select-start(ls-case-idx)
                   " : number of SELECTs: " ls-num-selects-disp
                   " : number of CASEs in SELECT: "
                       ls-num-cases-disp
                   " : nested level: " ls-nested-idx-disp
                   " : Line: " trim(l-src-code-str))
               end-call                
               goback 
           end-if 


           if ws-nested-idx > 0 and 
               upper-case(l-src-code-str(1:length(ws-end-select)))
               = ws-end-select                               
           then                                     
      *>      Get related start/end loop index based on current nest level.
               move ws-nested-select-idx(ws-nested-idx) 
                   to ls-end-select-idx               

               move l-cur-line-num 
                   to l-select-end(ls-end-select-idx)

               move l-cur-line-num to ls-cur-line-num-disp
               move l-num-selects to ls-num-selects-disp
               move ws-nested-idx to ls-nested-idx-disp
               call "logger" using concatenate(
                   "LOAD:PARSE-SELECTS :: found END SELECT at: " 
                   ls-cur-line-num-disp
                   " : SELECT idx: " ls-end-select-idx
                   " : related start line: " 
                       l-select-start(ls-end-select-idx)
                   " : number of SELECTs: " ls-num-selects-disp
                   " : nested level: " ls-nested-idx-disp 
                   " : Line: " trim(l-src-code-str)) 
               end-call 

               subtract 1 from ws-nested-idx               
           end-if 

           goback.

       end program parse-selects.
