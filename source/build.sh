
BUILD_STRING="cobc -O2 -x --debug -fstatic-call -lncurses \
    cobol_basic_interpreter.cbl \
    command_line_parser.cbl \
    logger.cbl \
    loading_parsing/load_program.cbl \
    loading_parsing/parse_loops.cbl \
    loading_parsing/parse_selects.cbl \
    loading_parsing/parse_subs.cbl \
    loading_parsing/parse_ifs.cbl \
    loading_parsing/parse_line_labels.cbl \
    variables/allocate_var.cbl \
    variables/array_indexed_name.cbl \
    variables/assign_var.cbl \
    variables/is_keyword.cbl \
    variables/repository/variable_repository.cbl \
    input/ascii_code_to_char_func.cbl \
    input/inkey_func.cbl \
    input/input_cmd.cbl  \
    output/clear_screen.cbl \
    output/paint_background.cbl \
    output/print_text.cbl \
    output/set_cursor_color.cbl \
    output/set_cursor_position.cbl \
    control_flow/sleep_program.cbl \
    control_flow/call_cmd.cbl \
    control_flow/gosub_goto_cmd.cbl \
    control_flow/if_handler.cbl \
    control_flow/select_handler.cbl \
    control_flow/conditional/conditional_processor.cbl \
    control_flow/conditional/conditional_statement_handler.cbl \
    control_flow/subroutine/gosub_return_handler.cbl \
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
