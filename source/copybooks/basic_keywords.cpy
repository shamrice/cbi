      ******************************************************************
      * Author: Erik Eriksen
      * Create Date: 2021-10-12
      * Last Modified: 2021-11-05
      * Purpose: Copybook containing definitions for implemented BASIC
      *          keywords. 
      * Tectonics: ./build.sh
      ******************************************************************

       78  ws-comment-rem             value "REM ".
       78  ws-comment-tic             value "'".
       
       78  ws-cls                     value "CLS ".
       78  ws-color                   value "COLOR ".
       78  ws-print                   value "PRINT ".
       78  ws-locate                  value "LOCATE ".       
       78  ws-end                     value "END ".
       78  ws-system                  value "SYSTEM ".
       78  ws-stop                    value "STOP ".
       78  ws-sleep                   value "SLEEP ".

       78  ws-dim                     value "DIM ".
       78  ws-dim-shared              value "DIM SHARED ".
       78  ws-integer-type            value "AS INTEGER".
       78  ws-string-type             value "AS STRING".

       78  ws-input                   value "INPUT ".

       78  ws-do                      value "DO ".
       78  ws-do-while                value "DO WHILE ".
       78  ws-do-until                value "DO UNTIL ".
       78  ws-loop                    value "LOOP ".
       78  ws-loop-while              value "LOOP WHILE ".
       78  ws-loop-until              value "LOOP UNTIL ".

       78  ws-while                   value "WHILE ".
       78  ws-wend                    value "WEND ".

       78  ws-for                     value "FOR ".
       78  ws-to                      value " TO ".
       78  ws-step                    value " STEP ".
       78  ws-next                    value "NEXT ".
       

       78  ws-sub                     value "SUB ".
       78  ws-end-sub                 value "END SUB".
       78  ws-call                    value "CALL ".
       78  ws-goto                    value "GOTO ".
       

      *>   Unused/not yet implemented keywords
       78  ws-if                      value "IF ".
       78  ws-elseif                  value "ELSEIF ".
       78  ws-end-if                  value "END IF".
       
       78  ws-select-case             value "SELECT CASE".
       78  ws-case                    value "CASE".
       78  ws-end-select              value "END SELECT".

       78  ws-const                   value "CONST ".

       78  ws-on-error                value "ON ERROR".

       78  ws-open                    value "OPEN".
       78  ws-close                   value "CLOSE".

       78  ws-declare                 value "DECLARE ".
       78  ws-return                  value "RETURN".
       78  ws-screen                  value "SCREEN".
       78  ws-width                   value "WIDTH".
       78  ws-line                    value "LINE ".
       78  ws-circle                  value "CIRCLE ".
       78  ws-sound                   value "SOUND".
       78  ws-play                    value "PLAY".
