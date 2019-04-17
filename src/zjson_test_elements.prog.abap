*&---------------------------------------------------------------------*
*& Report  ZJSON_TEST_ELEMENTS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT zjson_test_elements.

DATA: lr_runtime TYPE REF TO cl_rcc_runtime.

*PERFORM test_parser.

*PERFORM test_json_element.
PERFORM test_json_object.
*PERFORM test_json_array.
*PERFORM test_json_array_builder.
*PERFORM test_streams.



FORM test_parser.


  DATA: lv_json_str TYPE string.

  DATA: lt_app_set TYPE STANDARD TABLE OF zao_app_set.

  PERFORM runtime_init.
  SELECT * FROM zao_app_set INTO TABLE lt_app_set.
  WRITE: / 'Data selected from DB'.
  PERFORM runtime_print.

  PERFORM runtime_init.
  lv_json_str = zcl_json_parser=>to_json( data = lt_app_set ).
  WRITE:/ 'Appset Entries to JSON from ITAB', lv_json_str.
  PERFORM runtime_print.

  CLEAR lt_app_set.
  PERFORM runtime_init.
  zcl_json_parser=>from_json( EXPORTING json = lv_json_str CHANGING data = lt_app_set ).
  WRITE:/ 'Convert Back to Appset Entries to ITAB', lv_json_str.
  PERFORM runtime_print.

  DATA: lr_element TYPE REF TO zif_json_element.

*  lv_json_str = `{ "string": "some text", "int" : 123, "array": [ true , null , 444 ] }`.

  PERFORM runtime_init.
  zcl_json_parser=>from_json( EXPORTING json = lv_json_str CHANGING json_element = lr_element ).
  WRITE: / 'JSON To Json Elements', lv_json_str.
  PERFORM runtime_print.

  PERFORM runtime_init.
*  WRITE: / 'Original JSON', lv_json_str.
  lv_json_str = lr_element->to_string( ).
  WRITE: / 'Json Elements to JSON', lv_json_str.

  PERFORM runtime_print.
ENDFORM.                    "test_parser
*&---------------------------------------------------------------------*
*&      Form  test_streams
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM test_streams.
"   DATA: lr_str_writer TYPE REF TO cl_abap_string_c_writer,
"         lv_final_string TYPE string,
"         lv_tmp_str TYPE string.

"   DATA: lt_strings TYPE zstring_table,
"         ls_string LIKE LINE OF lt_strings,
"         lr_object TYPE REF TO zcl_json_object,
"         lr_array TYPE REF TO zcl_json_array.

"   DATA: json TYPE xstring,
"         lv_len TYPE i.

"   DATA json_writer TYPE REF TO cl_sxml_string_writer.

" **********************************************************************

"   PERFORM runtime_init.

"   DO 1000 TIMES.
"     MOVE sy-index TO lv_tmp_str.
"     CONCATENATE 'test ' lv_tmp_str cl_abap_char_utilities=>cr_lf INTO ls_string-zstring.
"     APPEND ls_string TO lt_strings.
"   ENDDO.

"   "abap to json

"   json_writer = cl_sxml_string_writer=>create( type = if_sxml=>co_xt_json ).
"   json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_linebreaks ).
"   json_writer->if_sxml_writer~set_option( option = if_sxml_writer=>co_opt_indent ).


"   CALL TRANSFORMATION id SOURCE strings = lt_strings
"                                   RESULT XML json_writer.


"   json = json_writer->get_output( ).

"   lv_len =  xstrlen( json ).


"   WRITE: / 'Execution Time (XML Transfermation) :', lv_len.
"   PERFORM runtime_print.

"   CLEAR lv_final_string.
" **********************************************************************


"   CREATE OBJECT lr_array.

" *  CREATE OBJECT lr_str_writer.

"   PERFORM runtime_init.

"   DO 1000 TIMES.
"     MOVE sy-index TO lv_tmp_str.
"     CONCATENATE 'test ' lv_tmp_str cl_abap_char_utilities=>cr_lf INTO lv_tmp_str.
"     CREATE OBJECT lr_object.
"     lr_object->add( iv_name = 'zstring' iv_value = lv_tmp_str ).
"     lr_array->add( ir_element = lr_object ).
"   ENDDO.


"   lv_final_string = lr_array->to_string( ir_stream = lr_str_writer ).

" *  lv_final_string = lr_str_writer->get_result_string( ).


"   lv_len =  strlen( lv_final_string ).

"   WRITE: / 'Execution Time (Array Element Stream) :', lv_len.
"   PERFORM runtime_print.

"   CLEAR lv_final_string.
" **********************************************************************

" **********************************************************************

"   PERFORM runtime_init.

"   DO 1000 TIMES.
"     MOVE sy-index TO lv_tmp_str.
"     CONCATENATE 'test ' lv_tmp_str cl_abap_char_utilities=>cr_lf INTO ls_string-zstring.
"     APPEND ls_string TO lt_strings.
"   ENDDO.

"   CREATE OBJECT lr_array
"     EXPORTING
"       it_table = lt_strings.

"   lv_final_string = lr_array->to_string( ).

"   lv_len =  strlen( lv_final_string ).

"   WRITE: / 'Execution Time (Array Table Ref) :', lv_len.
"   PERFORM runtime_print.

"   CLEAR lv_final_string.
" **********************************************************************


"   CREATE OBJECT lr_str_writer.

"   PERFORM runtime_init.

"   DO 3000 TIMES.
"     MOVE sy-index TO lv_tmp_str.
"     CONCATENATE 'test ' lv_tmp_str cl_abap_char_utilities=>cr_lf INTO lv_tmp_str.
"     lr_str_writer->write( lv_tmp_str ).
"   ENDDO.

"   lv_final_string = lr_str_writer->get_result_string( ).

"   lr_str_writer->close( ).

"   lv_len =  strlen( lv_final_string ).

"   WRITE: / 'Execution Time (Stream) :', lv_len.
"   PERFORM runtime_print.

"   CLEAR lv_final_string.

" **********************************************************************

"   PERFORM runtime_init.

"   DO 3000 TIMES.
"     MOVE sy-index TO lv_tmp_str.
"     CONCATENATE 'test ' lv_tmp_str cl_abap_char_utilities=>cr_lf INTO lv_tmp_str.
"     CONCATENATE lv_final_string lv_tmp_str INTO lv_final_string.
"   ENDDO.

"   lv_len =  strlen( lv_final_string ).

"   WRITE: / 'Execution Time (String Concat) :', lv_len.
"   PERFORM runtime_print.

"   CLEAR lv_final_string.



" *  WRITE: / 'Final String: ' , lv_final_string.

ENDFORM.                    "test_streams
*&---------------------------------------------------------------------*
*&      Form  test_json_array_builder
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM test_json_array_builder.
  WRITE: / 'Test - JSON Array Builder- Begin'.

  DATA: lv_int TYPE i VALUE -222,
      lv_float TYPE p LENGTH 5 DECIMALS 2 VALUE '33.44',
      lv_bool TYPE abap_bool VALUE abap_true,
      lv_str TYPE string VALUE 'some string'.

  DATA: lv_value TYPE string.

  DATA: lr_builder TYPE REF TO zcl_json_builder,
        lr_element TYPE REF TO zif_json_element.

  PERFORM runtime_init.

  lr_builder = zcl_json_builder=>create( zif_json_element=>c_type_array ). "builder starting with array

  lr_builder->start( ). "starts an json object
  lr_builder->add( iv_name = 'number' iv_value = lv_int ).


  lr_builder->add( iv_name = 'boolean' iv_value = lv_bool ).

  lr_builder->add( iv_name = 'float' iv_value = lv_float ).

  lr_builder->add( iv_name = 'string' iv_value = lv_str ).

  lr_builder->add( iv_name = 'null' ir_element = zcl_json_null=>gc_instance ).

  lr_builder->end( ). "ends json object

  lr_builder->add( iv_value = lv_float ).

  lr_builder->add( ir_element = zcl_json_null=>gc_instance ).

  lr_builder->add( iv_value = lv_bool ).

  lr_element ?= lr_builder->build( ).

  lv_value = lr_element->to_string( ).

  WRITE: / 'Value: ', lv_value.

  PERFORM runtime_print.
  WRITE: / 'Test - JSON Array Builder - End'.


ENDFORM.                    "test_json_array_builder
*&---------------------------------------------------------------------*
*&      Form  test_json_array
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM test_json_array.
  WRITE: / 'Test - JSON Array - Begin'.

  DATA: lv_int TYPE i VALUE -222,
      lv_float TYPE p LENGTH 5 DECIMALS 2 VALUE '33.44',
      lv_bool TYPE abap_bool VALUE abap_true,
      lv_str TYPE string VALUE 'some string'.

  DATA: lr_value TYPE REF TO zcl_json_primitive,
         lr_object TYPE REF TO zcl_json_object,
         lr_array TYPE REF TO zcl_json_array.

  DATA: lv_value TYPE string.

  PERFORM runtime_init.

  CREATE OBJECT lr_array.

  CREATE OBJECT lr_object.

  CREATE OBJECT lr_value
    EXPORTING
      ir_value = lv_int.

  lr_object->add( iv_name = 'number' ir_element = lr_value ).

  lr_object->add( iv_name = 'boolean' iv_value = lv_bool ).

  lr_object->add( iv_name = 'float' iv_value = lv_float ).

  lr_object->add( iv_name = 'string' iv_value = lv_str ).

  lr_object->add( iv_name = 'null' ir_element = zcl_json_null=>get_instance( ) ).

  lr_array->add( iv_value = lv_float ).

  lr_array->add( ir_element = lr_object ).

  lr_array->add( ir_element = zcl_json_null=>get_instance( ) ).

  lr_array->add( iv_value = lv_bool ).

  lv_value = lr_array->to_string( ).

  WRITE: / 'Value: ', lv_value.

  PERFORM runtime_print.
  WRITE: / 'Test - JSON Array - End'.


ENDFORM.                    "test_json_array
*&---------------------------------------------------------------------*
*&      Form  test_json_object
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM test_json_object.
  WRITE: / 'Test - JSON Object - Begin'.

  DATA: lv_int TYPE i VALUE -222,
      lv_float TYPE p LENGTH 5 DECIMALS 2 VALUE '33.44',
      lv_bool TYPE abap_bool VALUE abap_true,
      lv_str TYPE string VALUE 'some string',
      lv_date TYPE sydatum,
      lv_time TYPE syuzeit,
      lv_timestamp TYPE timestampl.


  DATA: lr_value TYPE REF TO zcl_json_primitive,
        lr_object TYPE REF TO zcl_json_object.

  DATA: lv_value TYPE string.

  lv_date = sy-datum.
  lv_time = sy-uzeit.
  GET TIME STAMP FIELD lv_timestamp.

  CREATE OBJECT lr_object.

  CREATE OBJECT lr_value
    EXPORTING
      ir_value = lv_int.

  lr_object->add( iv_name = 'number' ir_element = lr_value ).

  lr_object->add( iv_name = 'boolean' iv_value = lv_bool ).

  lr_object->add( iv_name = 'float' iv_value = lv_float ).

  lr_object->add( iv_name = 'string' iv_value = lv_str ).

  lr_object->add( iv_name = 'date' iv_value = lv_date ).
  lr_object->add( iv_name = 'time' iv_value = lv_time ).
  lr_object->add( iv_name = 'timeStamp' iv_value = lv_timestamp ).

  lr_object->add( iv_name = 'null' ir_element = zcl_json_null=>get_instance( ) ).

  lv_value = lr_object->to_string( ).

  WRITE: / 'Value: ', lv_value.

  WRITE: / 'Test - JSON Begin - End'.
ENDFORM.                    "test_json_object


*&---------------------------------------------------------------------*
*&      Form  test_json_element
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM test_json_element.

  WRITE: / 'Test - JSON Element - Begin'.

  DATA: lv_int TYPE i VALUE -222,
        lv_float TYPE p LENGTH 5 DECIMALS 2 VALUE '33.44',
        lv_bool TYPE abap_bool VALUE abap_true,
        lv_str TYPE string VALUE 'some string'.

  DATA: lr_value TYPE REF TO zcl_json_primitive.

  DATA: lv_value TYPE string.

  CREATE OBJECT lr_value
    EXPORTING
      ir_value = lv_int.

  lv_value = lr_value->to_string( ).

  WRITE: / 'Value: ', lv_value.

  WRITE: / 'Test - JSON Element - End'.

ENDFORM.                    "test_json_element

*&---------------------------------------------------------------------*
*&      Form  runtime_init
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM runtime_init.
  CREATE OBJECT lr_runtime.
  lr_runtime->set_accuracy( cl_rcc_runtime=>mc_accuracy_millisec ).
  lr_runtime->get_runtime( ).
ENDFORM.                    "runtime_init

*&---------------------------------------------------------------------*
*&      Form  runtime_print
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM runtime_print.
  DATA: lv_time TYPE int4.
  lv_time = lr_runtime->get_runtime( ).
  WRITE:/ 'Runtime (millis): ',lv_time.
ENDFORM.                    "runtime_print
