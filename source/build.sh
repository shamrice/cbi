
BUILD_STRING="cobc -O2 -x --debug -fstatic-call \
    cobol_basic_interpreter.cbl \
    command_line_parser.cbl \
    logger.cbl \
    loading_parsing/load_program.cbl \
    loading_parsing/parse_loops.cbl \
    loading_parsing/parse_subs.cbl \
    loading_parsing/parse_line_labels.cbl \
    variables/allocate_var.cbl \
    variables/assign_var.cbl \
    variables/is_keyword.cbl \
    variables/get_var_value.cbl \
    input/input_cmd.cbl  \
    output/print_text.cbl \
    output/set_cursor_color.cbl \
    output/set_cursor_position.cbl \
    control_flow/sleep_program.cbl \
    control_flow/conditional_processor.cbl \
    control_flow/call_cmd.cbl \
    control_flow/subroutine/sub_handler.cbl \
    control_flow/loop/loop_handler.cbl \
    control_flow/loop/for_loop_handler.cbl \
    -o ./bin/cbi" 

echo 
echo "Building COBOL BASIC Interpreter"
echo "--------------------------------"
echo "By: Erik Eriksen"
echo "https://github.com/shamrice/cbi"
echo 
echo $BUILD_STRING
echo     
$BUILD_STRING 
echo 
echo 
echo Done.
echo 
