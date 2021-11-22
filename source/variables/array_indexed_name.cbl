      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-11-22
      * Last Modified: 2021-11-22
      * Purpose: Sets the value of the variable passed in to the indexed
      *          array name. Replacing index variable with numeric value
      *          as needed.
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. array-indexed-name.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.           

       input-output section.
           
       data division.

       working-storage section.     
    
       local-storage section.

       copy "copybooks/local_storage/ls_variable.cpy".  

       01  ls-assignment-dest            pic x(1024).
       01  ls-array-name                 pic x(1024).

       01  ls-temp-index-var-name        pic x(1024).
       01  ls-temp-array-idx-name        pic x(1024).
       01  ls-temp-array-idx-value       pic z(15)9.
       
       linkage section.       

       01  l-array-name                  pic x(1024).        
       

       procedure division using 
           l-array-name.

       main-procedure.

           move upper-case(trim(l-array-name)) to ls-assignment-dest

           call "logger" using concatenate(
               "ARRAY-INDEXED-NAME :: Variable name: " 
               trim(ls-assignment-dest))
           end-call 


           unstring ls-assignment-dest 
               delimited by "("
               into ls-array-name ls-temp-index-var-name                              
           end-unstring 
        
           if ls-temp-index-var-name not = spaces then

               inspect ls-temp-index-var-name 
                   replacing all ")" by spaces 
                
               move trim(ls-temp-index-var-name) 
               to ls-temp-array-idx-name

               call "logger" using ls-temp-array-idx-name
               if trim(ls-temp-array-idx-name) not numeric then 

                   move ls-temp-array-idx-name to ls-variable-name
                   call "get-variable" using 
                       ls-variable ls-get-variable-return-code
                   end-call 
                   
                   if ls-get-variable-return-code > 0
                       and ls-type-integer
                   then                        

                       move ls-variable-value-num
                       to ls-temp-array-idx-value

                       move spaces to ls-assignment-dest
                       string
                           upper-case(trim(ls-array-name))
                           "("
                           trim(ls-temp-array-idx-value)
                           ")"
                           into ls-assignment-dest 
                       end-string 
                       
                       call "logger" using concatenate( 
                           "ARRAY-INDEXED-NAME :: Parsed array variable"
                           " name: "
                           trim(ls-assignment-dest))
                       end-call 
                   else 
                       call "logger" using concatenate( 
                           "ARRAY-INDEXED-NAME :: WARNING : Failed to "
                           "find "
                           " numeric array integer variable value for: "
                           trim(ls-variable-name) " : Destination "
                           "variable name will be unchanged: " 
                           trim(ls-assignment-dest))
                       end-call 
                   end-if 
               else 
                   call "logger" using concatenate(
                       "ARRAY-INDEXED-NAME :: Array variable index is "
                       "already "
                       "numeric and will be used as-is: "
                       trim(ls-assignment-dest))
                   end-call 
               end-if
           else 
               call "logger" using concatenate( 
                   "ARRAY-INDEXED-NAME :: Not array skipping: "
                   "assignment-dest: " trim(ls-assignment-dest)
                   " temp param val: " trim(ls-temp-index-var-name))
               end-call 
           end-if    

           move ls-assignment-dest to l-array-name

           goback.

       end program array-indexed-name.
