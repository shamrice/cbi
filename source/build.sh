

BUILD_STRING="cobc -O2 -x --debug -fstatic-call cobol_basic_interpreter.cbl loading_parsing/load_program.cbl loading_parsing/parse_loops.cbl loading_parsing/parse_subs.cbl command_line_parser.cbl logger.cbl variables/allocate_var.cbl variables/assign_var.cbl variables/is_keyword.cbl output/print_text.cbl output/set_cursor_color.cbl output/set_cursor_position.cbl control_flow/sleep_program.cbl control_flow/conditional_processor.cbl input/input_cmd.cbl -o ./bin/cbi "

echo $BUILD_STRING
echo 
$BUILD_STRING 
echo 
echo 
echo Done.
echo 
