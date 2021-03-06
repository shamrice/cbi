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

       01  ls-max-if-idx                 usage index.      

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
           set ls-max-if-idx to l-num-ifs 
           perform varying l-if-idx from 1 by 1 
           until l-if-idx > ls-max-if-idx 
               if l-if-start(l-if-idx) = l-cur-line-num then 
                   set l-if-not-processed(l-if-idx) to true                    
                   exit perform 
               end-if 
           end-perform 

           if l-if-idx > ls-max-if-idx then 
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
                   l-if-idx "******************"
               ) end-call 
               set l-if-processed(l-if-idx) to true                 
           else 

               evaluate true 
                   when l-num-elseifs(l-if-idx) > 0 
                       compute l-cur-line-num = 
                           l-elseif-start(l-if-idx, 1) - 1
                       end-compute 
                   
                   when l-else-start(l-if-idx) > 0 
                       compute l-cur-line-num = 
                           l-else-start(l-if-idx) - 1
                       end-compute                        

                   when other 
                *> Check if END IF exists before setting.
                       if l-if-end(l-if-idx) > 0 then 
                           compute l-cur-line-num = 
                               l-if-end(l-if-idx) - 1
                           end-compute                   
                       end-if 


               end-evaluate
           end-if             

           exit paragraph. 




       handle-elseif.

      *>   Find related if idx
           set ls-max-if-idx to l-num-ifs
           perform varying l-if-idx from 1 by 1 
           until l-if-idx > ls-max-if-idx

               perform varying l-elseif-idx from 1 by 1 
               until l-elseif-idx > l-num-elseifs(l-if-idx) 

                   if l-elseif-start
                       (l-if-idx, l-elseif-idx) 
                       = l-cur-line-num 
                   then      
                       call "logger" using concatenate(
                           "********* ELSEIF FOUND: " 
                           l-elseif-idx " ELSE ID: " 
                           l-if-idx)
                       end-call 
                       set ls-elseif-found to true              
                       exit perform 
                   end-if 
               end-perform

               if ls-elseif-found then 
                   exit perform 
               end-if  
           end-perform 

           if l-if-idx > ls-max-if-idx then 
               call "logger" using concatenate(
                   "IF-HANDLER :: WARNING: Could not related IF index "
                   "in the IF boundary table for ELSEIF. "
                   "Skipping conditional check.")
               end-call 
               goback 
           end-if 

      *>   If conditional was already processed, move to end of if block
           if l-if-processed(l-if-idx) then 
               call "logger" using concatenate(
                   "IF-HANDLER :: If alread processed. Moving to "
                   "END IF line.")
               end-call 
               
               compute l-cur-line-num = l-if-end(l-if-idx) - 1 

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
                   l-if-idx "******************"
               ) end-call 
               set l-if-processed(l-if-idx) to true  
           else 

               evaluate true 
                   when l-num-elseifs(l-if-idx) > l-elseif-idx 
                       compute l-cur-line-num = 
                           l-elseif-start(
                               l-if-idx, (l-elseif-idx + 1)) - 1
                       end-compute 
                   
                   when l-else-start(l-if-idx) > 0 
                       compute l-cur-line-num = 
                           l-else-start(l-if-idx) - 1
                       end-compute                        

                   when other 
                       compute l-cur-line-num = l-if-end(l-if-idx) - 1 

               end-evaluate
           end-if          

           exit paragraph. 

           

       handle-else.
      *>   Find related IF idx. If already processed, skip ELSE and move
      *>   to END IF.
           set ls-max-if-idx to l-num-ifs
           perform varying l-if-idx from 1 by 1 
           until l-if-idx > ls-max-if-idx 
               if l-else-start(l-if-idx) = l-cur-line-num then 
                   if l-if-processed(l-if-idx) then 
                       compute l-cur-line-num = l-if-end(l-if-idx) - 1 
                       goback 
                   end-if                    
               end-if 
           end-perform 

           exit paragraph.


       handle-end-if.
      *>   Find related IF idx for END IF and mark as unprocessed for
      *>   next potential iteration over IF block.
           set ls-max-if-idx to l-num-ifs
           perform varying l-if-idx from 1 by 1 
           until l-if-idx > ls-max-if-idx 
               if l-if-end(l-if-idx) = l-cur-line-num then 
                   set l-if-not-processed(l-if-idx) to true  
                   goback                    
               end-if 
           end-perform 

           exit paragraph.

       end program if-handler.
