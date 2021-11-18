      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-05
      * Last Modified: 2021-11-18
      * Purpose: Handles a IF statements and moves current line number 
      *          to the correct location based on processing of the 
      *          statement.      
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. if-handler.

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
      
       01  ls-line-text                  pic x(1024).

       01  ls-line-text-temp             pic x(1024).

       01  ls-conditional-ret-val        pic 9.       

       01  ls-if-idx                     pic 9(4) comp.
       01  ls-elseif-idx                 pic 99 comp.

       01  ls-elseif-found-sw            pic a value 'N'.
           88  ls-elseif-found           value 'Y'.
           88  ls-elseif-not-found       value 'N'.
       
       linkage section.       

       01  l-src-code-str                pic x(1024). 

       01  l-cur-line-num                pic 9(5) comp.

       copy "copybooks/linkage_section/l_if_boundary_table.cpy".       
                   

       procedure division using 
           l-src-code-str l-cur-line-num 
           l-if-boundary-table.

       main-procedure.

      *> TODO: Keywords need to be uppercase. Setting the whole line 
      *>       to uppercase breaks string comparisons. 
      *     move upper-case(trim(l-src-code-str))
           move trim(l-src-code-str)
               to ls-line-text

           call "logger" using concatenate(
               "IF-HANDLER :: Evaluating statement: "
               trim(ls-line-text))
           end-call

      *>   Remove ending "THEN" if exists...
           move reverse(ls-line-text) to ls-line-text-temp

           inspect ls-line-text-temp  
           replacing first "NEHT" by spaces            

           move reverse(ls-line-text-temp) to ls-line-text 

           evaluate true 
               when ls-line-text(1:length(ws-if)) 
                   = ws-if             
                   perform handle-if               

               when ls-line-text(1:length(ws-elseif))
                   = ws-elseif 
                   perform handle-elseif               

               when ls-line-text(1:length(ws-else))
                   = ws-else 
                   perform handle-else

               when ls-line-text(1:length(ws-end-if))
                   = ws-end-if 
                   perform handle-end-if                 

           end-evaluate

           goback.

       
       handle-if.

      *>   Find related if idx and set to not processed yet.
           perform varying ls-if-idx from 1 by 1 
           until ls-if-idx > l-num-ifs 
               if l-if-start(ls-if-idx) = l-cur-line-num then 
                   set l-if-not-processed(ls-if-idx) to true                    
                   exit perform 
               end-if 
           end-perform 

           if ls-if-idx > l-num-ifs then 
               call "logger" using concatenate(
                   "IF-HANDLER :: WARNING: Could not related IF index "
                   "in the IF boundary table. Skipping conditional "
                   "check.")
               end-call 
               goback 
           end-if 


           inspect ls-line-text 
           replacing first ws-if by spaces 

           call "conditional-statement-handler" using 
               ls-line-text               
               ls-conditional-ret-val
           end-call 

      *> If true, move to next line, mark if condition as handled. when 
      *> reaches next elseif or else, will jump to end-if 
      
      *> if false, move to next elseif or else (if exists), if not, 
      *> move to end if.
           if ls-conditional-ret-val > 0 then 
               call "logger" using concatenate(
                   "***********SETTING IF TO PROCESSED FOR ID: "
                   ls-if-idx "******************"
               ) end-call 
               set l-if-processed(ls-if-idx) to true                 
           else 

               evaluate true 
                   when l-num-elseifs(ls-if-idx) > 0 
                       compute l-cur-line-num = 
                           l-elseif-start(ls-if-idx, 1) - 1
                       end-compute 
                   
                   when l-else-start(ls-if-idx) > 0 
                       compute l-cur-line-num = 
                           l-else-start(ls-if-idx) - 1
                       end-compute                        

                   when other 
                *> Check if END IF exists before setting.
                       if l-if-end(ls-if-idx) > 0 then 
                           compute l-cur-line-num = 
                               l-if-end(ls-if-idx) - 1
                           end-compute                   
                       end-if 


               end-evaluate
           end-if             

           exit paragraph. 




       handle-elseif.

      *>   Find related if idx
           perform varying ls-if-idx from 1 by 1 
           until ls-if-idx > l-num-ifs

               perform varying ls-elseif-idx from 1 by 1 
               until ls-elseif-idx > l-num-elseifs(ls-if-idx) 

                   if l-elseif-start
                       (ls-if-idx, ls-elseif-idx) 
                       = l-cur-line-num 
                   then      
                       call "logger" using concatenate(
                           "********* ELSEIF FOUND: " 
                           ls-elseif-idx " ELSE ID: " 
                           ls-if-idx)
                       end-call 
                       set ls-elseif-found to true              
                       exit perform 
                   end-if 
               end-perform

               if ls-elseif-found then 
                   exit perform 
               end-if  
           end-perform 

           if ls-if-idx > l-num-ifs then 
               call "logger" using concatenate(
                   "IF-HANDLER :: WARNING: Could not related IF index "
                   "in the IF boundary table for ELSEIF. "
                   "Skipping conditional check.")
               end-call 
               goback 
           end-if 

      *>   If conditional was already processed, move to end of if block
           if l-if-processed(ls-if-idx) then 
               call "logger" using concatenate(
                   "IF-HANDLER :: If alread processed. Moving to "
                   "END IF line.")
               end-call 
               
               compute l-cur-line-num = l-if-end(ls-if-idx) - 1 

               goback 
           end-if 

           inspect ls-line-text 
           replacing first ws-elseif by spaces 

           call "conditional-statement-handler" using 
               ls-line-text               
               ls-conditional-ret-val
           end-call 
       
      *> If true, move to next line. mark if condition as handled. when 
      *> reaches next elseif or else, will jump to end if.

      *> if false, move to next elseif or else (if exists). If not, move
      *> to end if.
           if ls-conditional-ret-val > 0 then 
               call "logger" using concatenate(
                   "***********SETTING IF TO PROCESSED FOR ID: "
                   ls-if-idx "******************"
               ) end-call 
               set l-if-processed(ls-if-idx) to true  
           else 

               evaluate true 
                   when l-num-elseifs(ls-if-idx) > ls-elseif-idx 
                       compute l-cur-line-num = 
                           l-elseif-start(
                               ls-if-idx, (ls-elseif-idx + 1)) - 1
                       end-compute 
                   
                   when l-else-start(ls-if-idx) > 0 
                       compute l-cur-line-num = 
                           l-else-start(ls-if-idx) - 1
                       end-compute                        

                   when other 
                       compute l-cur-line-num = l-if-end(ls-if-idx) - 1 

               end-evaluate
           end-if          

           exit paragraph. 

           

       handle-else.
      *>   Find related IF idx. If already processed, skip ELSE and move
      *>   to END IF.
           perform varying ls-if-idx from 1 by 1 
           until ls-if-idx > l-num-ifs 
               if l-else-start(ls-if-idx) = l-cur-line-num then 
                   if l-if-processed(ls-if-idx) then 
                       compute l-cur-line-num = l-if-end(ls-if-idx) - 1 
                       goback 
                   end-if                    
               end-if 
           end-perform 

           exit paragraph.


       handle-end-if.
      *>   Find related IF idx for END IF and mark as unprocessed for
      *>   next potential iteration over IF block.
           perform varying ls-if-idx from 1 by 1 
           until ls-if-idx > l-num-ifs 
               if l-if-end(ls-if-idx) = l-cur-line-num then 
                   set l-if-not-processed(ls-if-idx) to true  
                   goback                    
               end-if 
           end-perform 

           exit paragraph.

       end program if-handler.
