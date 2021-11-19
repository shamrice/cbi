      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-13
      * Last Modified: 2021-11-19
      * Purpose: Handles a SELECT statements and moves current line number 
      *          to the correct location based on processing of the 
      *          statement.      
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. select-handler.

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
      
       copy "copybooks/local_storage/ls_variable.cpy".  

       01  ls-max-select-idx             usage index.

       01  ls-check-val-num-temp         pic 9(16).
       01  ls-case-val-num-temp          pic 9(16).       

       01  ls-line-text                  pic x(1024).

       01  ls-line-text-temp             pic x(1024).

       01  ls-conditional-ret-val        pic 9.       

       01  ls-conditional-operator-count pic 9(4) comp.

       01  ls-case-found-sw              pic a value 'N'.
           88  ls-case-found             value 'Y'.
           88  ls-case-not-found         value 'N'.

       01  ls-check-val-numeric-sw       pic a value 'N'.
           88  ls-check-val-numeric      value 'Y'.
           88  ls-check-val-not-numeric  value 'N'.
       
       linkage section.       

       01  l-src-code-str                pic x(1024). 

       01  l-cur-line-num                pic 9(5) comp.

       copy "copybooks/linkage_section/l_select_boundary_table.cpy".       
                   

       procedure division using 
           l-src-code-str l-cur-line-num 
           l-select-boundary-table.   

       main-procedure.

      *> TODO: Keywords need to be uppercase. Setting the whole line 
      *>       to uppercase breaks string comparisons. 
           move upper-case(trim(l-src-code-str))
               to ls-line-text

           call "logger" using concatenate(
               "SELECT-HANDLER :: Evaluating statement: "
               trim(ls-line-text))
           end-call      

           evaluate true 
               when ls-line-text(1:length(ws-select-case)) 
                   = ws-select-case             
                   perform handle-select-case              

               when ls-line-text(1:length(ws-case))
                   = ws-case 
                   perform handle-case               

               when ls-line-text(1:length(ws-end-select))
                   = ws-end-select 
                   perform handle-end-select

           end-evaluate

           goback.

       
       handle-select-case.

      *>   Find related select idx and set to not processed yet.
           set ls-max-select-idx to l-num-selects
           perform varying l-select-idx from 1 by 1 
           until l-select-idx > ls-max-select-idx 
               if l-select-start(l-select-idx) = l-cur-line-num then 
                   set l-select-not-processed(l-select-idx) to true                    
                   exit perform 
               end-if 
           end-perform 

           if l-select-idx > ls-max-select-idx then 
               call "logger" using concatenate(
                   "SELECT-HANDLER :: WARNING: Could not find related "
                   "index in the SELECT boundary table. Skipping "
                   "SELECT CASE check.")
               end-call 
               goback 
           end-if 


           inspect ls-line-text 
           replacing first ws-select-case by spaces 

           inspect ls-line-text 
               tallying ls-conditional-operator-count
               for all ">"
                   all "<"
                   all "="                   
                   all "AND"
                   all "OR"
                   all "NOT"                   
           
      *> If expression is a conditional, evaluate that and set the 
      *> TRUE/FALSE values accordingly. 

      *> TODO : add check for equations as well or string concats.
           if ls-conditional-operator-count > 0 then 
               call "conditional-statement-handler" using 
                   ls-line-text                   
                   ls-conditional-ret-val
               end-call 

               move ls-conditional-ret-val 
               to l-select-check-val(l-select-idx)      
           else 

      *>   Check if variable, if so replace it with that value, if not,
      *>   leave value as written.
               move ls-line-text to ls-variable-name 
               call "get-variable" using 
                   ls-variable 
                   ls-get-variable-return-code
               end-call 
      
               if ls-get-variable-return-code > 0 then       
                   if ls-type-integer then 
                       move ls-variable-value-num
                       to l-select-check-val(l-select-idx)
                   else     
                       move ls-variable-value
                       to l-select-check-val(l-select-idx)
                   end-if 
               else                
      *>           Replace any leading or trailing double quotes.
                   inspect ls-line-text replacing leading '"' by spaces
                   move reverse(ls-line-text) to ls-line-text-temp
                   inspect ls-line-text-temp replacing leading '"' 
                       by spaces
                   move reverse(ls-line-text-temp) to ls-line-text 

      *>           Set value.
                   move trim(ls-line-text)     
                   to l-select-check-val(l-select-idx)
               end-if 
               
           end-if 
           
      *>   Move to first CASE statement if exists, or to end of 
      *>   select if it doesn't. 
           if l-num-cases(l-select-idx) > 0 then 
               compute l-cur-line-num = 
                   l-case-start(l-select-idx, 1) - 1
               end-compute 
           else 
      *>       If END SELECT is missing (bad syntax), will produce 
      *>       'undefined' behavior. (Will move to line 99999)
               compute l-cur-line-num = 
                   l-select-end(l-select-idx) - 1 
               end-compute 
           end-if   

           exit paragraph. 




       handle-case.

      *>   Find related if idx
           set ls-max-select-idx to l-num-selects
           perform varying l-select-idx from 1 by 1 
           until l-select-idx > ls-max-select-idx

               perform varying l-case-idx from 1 by 1 
               until l-case-idx > l-num-cases(l-select-idx) 

                   if l-case-start(l-select-idx, l-case-idx) 
                       = l-cur-line-num 
                   then      
                       call "logger" using concatenate(
                           "********* CASE FOUND: " 
                           l-case-idx " SELECT ID: " 
                           l-select-idx)
                       end-call 
                       set ls-case-found to true              
                       exit perform 
                   end-if 
               end-perform

               if ls-case-found then 
                   exit perform 
               end-if  
           end-perform 

           if l-select-idx > ls-max-select-idx then 
               call "logger" using concatenate(
                   "SELECT-HANDLER :: WARNING: Could not related SELECT"
                   " index in the SELECT boundary table for CASE. "
                   "Skipping conditional check.")
               end-call 
               goback 
           end-if 

      *>   If case was already processed, move to end of select block
           if l-select-processed(l-select-idx) then 
               call "logger" using concatenate(
                   "SELECT-HANDLER :: select alread processed. Moving "
                   "to END SELECT line.")
               end-call 
               
               compute l-cur-line-num = l-select-end(l-select-idx) - 1 

               goback 
           end-if 


      *>   If currently on CASE ELSE and not processed, always true.
           if trim(ls-line-text) = ws-case-else then 
               set l-select-processed(l-select-idx) to true 
               goback 
           end-if 


      *>   Check value of CASE. Move to next line if true or jump to 
      *>   next CASE or END SELECT if false.
           inspect ls-line-text 
           replacing first ws-case by spaces 

      *>   TODO : CASE can have conditionals (REQUIRES 'IS') and 
      *>          equations, not just simple single values. 
           
           if trim(l-select-check-val(l-select-idx)) is numeric then 
               move trim(l-select-check-val(l-select-idx)) 
               to ls-check-val-num-temp

               set ls-check-val-numeric to true 
           end-if 


           move ls-line-text to ls-variable-name 
           call "get-variable" using 
               ls-variable 
               ls-get-variable-return-code
           end-call 

      *>   Only TRUE cases are checked here. If found, SELECT is marked
      *>   as processed and flow will move to next line.      
           if ls-get-variable-return-code > 0 then       
                if ls-type-integer and ls-check-val-numeric then       
                   if ls-variable-value-num = ls-check-val-num-temp then 
                       set l-select-processed(l-select-idx) to true 
                       goback 
                   end-if 
               else       
                   if ls-variable-value = 
                       l-select-check-val(l-select-idx)       
                   then 
                       set l-select-processed(l-select-idx) to true 
                       goback 
                   end-if 
               end-if 
           else 
               if trim(ls-line-text) is numeric and ls-check-val-numeric
               then 
                   move trim(ls-line-text) to ls-case-val-num-temp
                   if ls-case-val-num-temp = ls-check-val-num-temp then 
                       set l-select-processed(l-select-idx) to true 
                       goback 
                   end-if 
               else 
                   if trim(ls-line-text) 
                       = l-select-check-val(l-select-idx) 
                   then 
                       set l-select-processed(l-select-idx) to true 
                       goback 
                   end-if 
               end-if 
           end-if 

      *>   If made this far, CASE check failed, move to next CASE or 
      *>   END SELECT.
           if l-num-cases(l-select-idx) > l-case-idx then 
               compute l-cur-line-num = 
                   l-case-start(l-select-idx, l-case-idx + 1)  - 1
               end-compute 
           else 
               compute l-cur-line-num = l-select-end(l-select-idx) - 1 
           end-if                

           exit paragraph. 

           


       handle-end-select.
      *>   Find related SELECT idx for END SELECT and mark as unprocessed
      *>   for next potential iteration over SELECT block.
           set ls-max-select-idx to l-num-selects
           perform varying l-select-idx from 1 by 1 
           until l-select-idx > ls-max-select-idx
               if l-select-end(l-select-idx) = l-cur-line-num then 
                   set l-select-not-processed(l-select-idx) to true  
                   goback                    
               end-if 
           end-perform 

           exit paragraph.

       end program select-handler.
