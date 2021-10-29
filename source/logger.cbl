      *>*****************************************************************
      *> Author: Erik Eriksen
      *> Create Date: 2020-12-26 (originally for CRSSR)
      *> Last Updated: 2021-10-28
      *> Purpose: File logger for cobol-basic-interpreter
      *> Tectonics:
      *>     ./build.sh
      *>*****************************************************************
       replace ==:BUFFER-SIZE:== by ==32768==.

       identification division.
       program-id. logger.

       environment division.

       configuration section.

       input-output section.
           file-control.
               select optional fd-log-file
               assign to dynamic ws-file-name
               organization is line sequential.

       data division.

       file section.
           FD fd-log-file.
           01 f-log-text-raw             pic x(:BUFFER-SIZE:).


       working-storage section.

       01  ws-date-record.
           05  ws-current-date.
               10  ws-year               pic 9(4).
               10  ws-month              pic 99.
               10  ws-day                pic 99.
           05 ws-current-time.
               10  ws-hour               pic 99.
               10  ws-min                pic 99.
               10  ws-sec                pic 99.
               10  ws-milli              pic 99.
           05  ws-time-offset            pic S9(4).

       01  ws-log-enabled-sw             pic a value 'N'.
           88  ws-log-enabled            value 'Y'.
           88  ws-log-disabled           value 'N'.

       77  ws-log-buffer                 pic x(:BUFFER-SIZE:).

       77  ws-file-name                  pic x(18) 
                                         value "cbi_UNSET.log".   

       linkage section.
       01  l-log-text                    pic x any length.


       procedure division using l-log-text.

       main-procedure.

           if ws-log-disabled then 
               goback 
           end-if 

           move spaces to ws-log-buffer
           move function current-date to ws-date-record

      * Build formatted log line for output.         
           string 
               "[" delimited by size
               ws-year delimited by size 
               "-" delimited by size 
               ws-month delimited by size
               "-" delimited by size 
               ws-day delimited by size 
               " " delimited by size
               ws-hour delimited by size
               ":" delimited by size
               ws-min delimited by size
               ":" delimited by size
               ws-sec delimited by size
               "." delimited by size
               ws-milli delimited by size
               "] " delimited by size
               l-log-text delimited by size
               into ws-log-buffer
           end-string
           
           write f-log-text-raw from ws-log-buffer           

           goback.
       


      *>*****************************************************************
      *> Author: Erik Eriksen
      *> Create Date: 2021-10-28
      *> Last Updated: 2021-10-28
      *> Purpose: Enables logging, sets file name, and opens the log 
      *>          file for writing.
      *> Tectonics:
      *>     ./build.sh
      *>*****************************************************************
       entry "enable-logger".
           set ws-log-enabled to true 

      *> Dynamically create log file name using date as file name.
           move function current-date to ws-date-record
      
           string
               "cbi" delimited by size
               "_" delimited by size  
               ws-year delimited by size
               ws-month delimited by size
               ws-day delimited by size 
               ".log" delimited by size 
               into ws-file-name
           end-string

           open extend fd-log-file
           goback.



      *>*****************************************************************
      *> Author: Erik Eriksen
      *> Create Date: 2021-10-28
      *> Last Updated: 2021-10-28
      *> Purpose: Disables the logging flag and closes the log file.
      *> Tectonics:
      *>     ./build.sh
      *>*****************************************************************
       entry "disable-logger".
           set ws-log-disabled to true 
           close fd-log-file
           goback.

       end program logger.
