

BUILD_STRING="cobc -O2 -x --debug -fstatic-call cobol_basic_interpreter.cbl logger.cbl variables/allocate_var.cbl variables/assign_var.cbl output/set_cursor_color.cbl output/set_cursor_position.cbl -o ./bin/cbi "

echo $BUILD_STRING
echo 
$BUILD_STRING 
echo 
echo 
echo Done.
echo 
