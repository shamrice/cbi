      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-13
      * Last Modified: 2021-10-28
      * Purpose: Parses command line arguements
      * Tectonics: ./build.sh
      ******************************************************************
       identification division.
       program-id. command-line-parser.

       environment division.
       
       configuration section.

       repository. 
           function all intrinsic.          

       special-names.

       input-output section.
 
       data division.
       file section.

       working-storage section.

       01  ws-temp-param                pic x(512).
       01  ws-param-pointer             pic 9(4) comp.       

       78  ws-help-param                value "--HELP".
       78  ws-list-param                value "--LIST".
       78  ws-run-param                 value "--RUN".
       78  ws-logging-param             value "--LOGGING".

       local-storage section.
       
       linkage section.    

       01  l-command-line-args          pic x(2024).
    
       01  l-input-source-file-name     pic x(1024).

       01  l-list-program-sw            pic a.
           88  l-list-program           value 'Y'.
           88  l-not-list-program       value 'N'.

       01  l-run-program-sw             pic a.
           88  l-run-program            value 'Y'.
           88  l-not-run-program        value 'N'.

       01  l-logging-sw                 pic a.
           88  l-enable-logging         value 'Y'.
           88  l-disable-logging        value 'N'.

       procedure division using 
           l-command-line-args l-input-source-file-name 
           l-list-program-sw l-run-program-sw l-logging-sw.  

       main-procedure.

           move 1 to ws-param-pointer
           
           perform until ws-param-pointer > length(l-command-line-args)
               unstring l-command-line-args 
                   delimited by spaces 
                   into ws-temp-param 
                   with pointer ws-param-pointer
               end-unstring
               
               if ws-temp-param not = spaces then 
                   perform parse-arg
               end-if 
           end-perform 

           if l-input-source-file-name = spaces then 
               display "ERROR: A source file is required."
               display space 
               perform display-help               
           end-if 

           if l-not-list-program and l-not-run-program then 
               set l-run-program to true 
           end-if 

           goback.


       parse-arg.

           evaluate upper-case(trim(ws-temp-param))

               when ws-help-param
                   perform display-help 

               when ws-run-param
                   set l-run-program to true 

               when ws-list-param
                   set l-list-program to true 

               when ws-logging-param
                   set l-enable-logging to true 

               when other 
                   move trim(ws-temp-param) to l-input-source-file-name
                
           end-evaluate 

           exit paragraph.      


       display-help.           
           display "Usage: cbi [OPTION]... [FILE]"
           display space 
           display "Options:"
           display 
               "      --run - Run the program after loading without "
               "prompt. (Default if no parameters are entered.)"
           end-display 
           display 
               "     --list - Print out program source code to display."
           end-display 
           display 
               "  --logging - Turns on interpreter logging while "
               "running and/or listing a program."
           end-display 
           display 
               "     --help - This help text."
           display spaces 
           display 
               "  [FILE] - File name of BASIC program to list "
               "and/or run."
           end-display 
           display spaces 
           stop run 

           exit paragraph.

       end program command-line-parser.
