class ZCL_JSON_PARSER definition
  public
  final
  create private .

public section.
  type-pools ABAP .

  constants:
    BEGIN OF pretty_mode,
          none          TYPE char1  VALUE '',
          low_case      TYPE char1  VALUE 'L',
          camel_case    TYPE char1  VALUE 'X',
        END OF  pretty_mode .

  class-methods STRING_TO_XSTRING
    importing
      !IN type STRING
    changing
      !OUT type ANY .
  class-methods XSTRING_TO_STRING
    importing
      !IN type ANY
    returning
      value(OUT) type STRING .
  class-methods TO_JSON
    importing
      !DATA type DATA optional
      !JSON_ELEMENT type ref to ZIF_JSON_ELEMENT optional
      !COMPRESS type ABAP_BOOL default ABAP_FALSE
      !NAME type STRING optional
      !PRETTY_NAME type CHAR1 default PRETTY_MODE-NONE
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
    returning
      value(R_JSON) type STRING .
  class-methods FROM_JSON
    importing
      !JSON type STRING
      !PRETTY_NAME type CHAR1 default PRETTY_MODE-NONE
      !SPECIAL_TOKEN_BOUNDS type STRING optional
      !OFFSET type OFFSET optional
    changing
      !DATA type DATA optional
      !JSON_ELEMENT type ref to ZIF_JSON_ELEMENT optional
      !PARSER type ref to ZCL_JSON_PARSER optional .
  methods RESTORE
    importing
      !NAME type STRING optional
    changing
      !DATA type DATA optional .
  class-methods CLASS_CONSTRUCTOR .
  methods GET_OFFSET
    returning
      value(RV_OFFSET) type OFFSET .
protected section.
private section.

  data MR_DATA type ref to DATA .
  data MR_ELEMENT type ref to ZIF_JSON_ELEMENT .
  data MV_PARSE_TO_DATA type ABAP_BOOL value ABAP_FALSE. "#EC NOTEXT
  data MV_JSON type STRING .
  data MV_JSON_LENGTH type I .
  data MV_OFFSET type I .
  data MV_PRITTY_NAME_MODE type CHAR1 value PRETTY_MODE-NONE. "#EC NOTEXT
  class-data MV_WHITE_SPACE type STRING .
  data MR_JSON_BUILDER type ref to ZCL_JSON_BUILDER .
  data MV_JSON_TOKEN_BOUNDS type STRING value ` []{}:"`. "#EC NOTEXT
  data MV_IGNORE_COLON type ABAP_BOOL value ABAP_FALSE. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_JSON_PARSER IMPLEMENTATION.


METHOD class_constructor.
  mv_white_space = cl_abap_char_utilities=>get_simple_spaces_for_cur_cp( ).
ENDMETHOD.


METHOD from_json.

  DATA: lr_parser TYPE REF TO Zcl_json_parser.


  DATA: length    TYPE i,
    unescaped LIKE json.

  CREATE OBJECT lr_parser.

  IF json IS NOT INITIAL.
    lr_parser->mv_json = json.

    " to eliminate numeric replacement calls for every single sting value, we do
    " replacement over all JSON text, while this shall not destroy JSON structure
    REPLACE ALL OCCURRENCES OF `\r\n` IN lr_parser->mv_json WITH cl_abap_char_utilities=>cr_lf.
    REPLACE ALL OCCURRENCES OF `\n`   IN lr_parser->mv_json WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF `\t`   IN lr_parser->mv_json WITH cl_abap_char_utilities=>horizontal_tab.
*   REPLACE ALL OCCURRENCES OF `\f`   IN r_json WITH cl_abap_char_utilities=>form_feed.
*   REPLACE ALL OCCURRENCES OF `\b`   IN r_json WITH cl_abap_char_utilities=>backspace.

    lr_parser->mv_json_length = numofchar( lr_parser->mv_json ).

  ENDIF.

  IF special_token_bounds IS NOT INITIAL.
    CONCATENATE lr_parser->mv_json_token_bounds special_token_bounds INTO lr_parser->mv_json_token_bounds.
  ENDIF.

  lr_parser->mv_offset = offset.
  lr_parser->mv_pritty_name_mode = pretty_name.

  IF data IS SUPPLIED.
    lr_parser->mv_parse_to_data = abap_true.

    lr_parser->restore( CHANGING data = data ).
  ELSE.

    lr_parser->restore( ).

    json_element = lr_parser->mr_json_builder->build( ).

  ENDIF.

  parser = lr_parser.

ENDMETHOD.


METHOD get_offset.
  rv_offset = mv_offset.
ENDMETHOD.


METHOD restore.

  DATA: mark        LIKE mv_offset,
        match       LIKE mv_offset,
        pos         LIKE mv_offset,
        excp        TYPE REF TO cx_sy_move_cast_error,
        name_json   TYPE string,
        name_abap   TYPE string,
        sdummy      TYPE string,
        bdummy      TYPE abap_bool,
        lr_idummy   TYPE REF TO i,
        lr_bdummy   TYPE REF TO abap_bool,
        lr_sdummy   TYPE REF TO string,
        line        TYPE REF TO data,
        elem_descr  TYPE REF TO cl_abap_elemdescr,
        type_descr  TYPE REF TO cl_abap_typedescr,
        table_descr TYPE REF TO cl_abap_tabledescr,
        lr_exroot   TYPE REF TO cx_root,
        lr_exjson   TYPE REF TO Zcx_json_exception.

  FIELD-SYMBOLS: <line>           TYPE any,
                 <table>          TYPE ANY TABLE,
                 <table_sorted>   TYPE SORTED TABLE,
                 <table_hashed>   TYPE HASHED TABLE,
                 <table_standard> TYPE STANDARD TABLE,
                 <value> TYPE any.

  eat_white.

  TRY .
      CASE mv_json+mv_offset(1).

        WHEN `{`. "object

          IF mv_parse_to_data EQ abap_false.
            IF mr_json_builder IS NOT BOUND.
              mr_json_builder = Zcl_json_builder=>create( ). "start with object
            ELSE.
              mr_json_builder->start( iv_name = name ). "start object
            ENDIF.
          ENDIF.

          eat_char `{`.
          WHILE mv_offset < mv_json_length AND mv_json+mv_offset(1) NE `}`.

            eat_white.
            eat_string name_json.
            eat_white.
            eat_char `:`.
            eat_white.
            UNASSIGN <value>.

            name_abap = name_json.

            IF mv_parse_to_data EQ abap_true.

              TRANSLATE name_abap TO UPPER CASE.
              ASSIGN COMPONENT name_abap OF STRUCTURE data TO <value>.

              IF <value> IS NOT ASSIGNED AND mv_pritty_name_mode EQ abap_true.
                name_abap = name_json.
                REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN name_abap WITH `$1_$2`. "#EC NOTEXT
                TRANSLATE name_abap TO UPPER CASE.
                ASSIGN COMPONENT name_abap OF STRUCTURE data TO <value>.
              ENDIF.

              IF <value> IS ASSIGNED.
                restore( CHANGING data = <value> ).
              ENDIF.

            ELSE.
              restore( name = name_abap ).
            ENDIF.

            eat_white.

            IF mv_offset < mv_json_length AND mv_json+mv_offset(1) NE `}`.
              eat_char `,`.
            ELSE.
              EXIT.
            ENDIF.

          ENDWHILE.

          eat_char `}`.

          IF mv_parse_to_data EQ abap_false.
            mr_json_builder->end( ). "end object
          ENDIF.

        WHEN `[`. "array

          IF mv_parse_to_data EQ abap_false.
            IF mr_json_builder IS NOT BOUND.
              mr_json_builder = Zcl_json_builder=>create( Zif_json_element=>c_type_array ). "start with array
            ELSE.
              mr_json_builder->start( iv_type = Zif_json_element=>c_type_array iv_name = name ). "start array
            ENDIF.
          ENDIF.

          eat_char `[`.
          eat_white.

          IF mv_json+mv_offset(1) NE `]`.

            IF mv_parse_to_data EQ abap_true.
              type_descr = cl_abap_typedescr=>describe_by_data( data ).
              IF type_descr->kind EQ cl_abap_typedescr=>kind_table.
                table_descr ?= type_descr.
                ASSIGN data TO <table>.
                CREATE DATA line LIKE LINE OF <table>.
                ASSIGN line->* TO <line>.
                WHILE mv_offset < mv_json_length AND mv_json+mv_offset(1) NE `]`.
                  CLEAR <line>.
                  restore(  CHANGING data = <line> ).
                  CASE table_descr->table_kind.
                    WHEN cl_abap_tabledescr=>tablekind_sorted.
                      ASSIGN data TO <table_sorted>.
                      INSERT <line> INTO TABLE <table_sorted>.
                    WHEN cl_abap_tabledescr=>tablekind_hashed.
                      ASSIGN data TO <table_hashed>.
                      INSERT <line> INTO TABLE <table_hashed>.
                    WHEN OTHERS.
                      ASSIGN data TO <table_standard>.
                      APPEND <line> TO <table_standard>.
                  ENDCASE.
                  eat_white.
                  IF mv_offset < mv_json_length AND mv_json+mv_offset(1) NE `]`.
                    eat_char `,`.
                  ELSE.
                    EXIT.
                  ENDIF.
                ENDWHILE.

              ELSE.
                " skip array
                WHILE mv_offset < mv_json_length AND mv_json+mv_offset(1) NE `}`.
                  eat_white.
                  restore( ).
                  IF mv_offset < mv_json_length AND mv_json+mv_offset(1) NE `]`.
                    eat_char `,`.
                  ELSE.
                    EXIT.
                  ENDIF.
                ENDWHILE.
              ENDIF.

            ELSE.

              WHILE mv_offset < mv_json_length AND mv_json+mv_offset(1) NE `]`.
                restore( ).
                eat_white.
                IF mv_offset < mv_json_length AND mv_json+mv_offset(1) NE `]`.
                  eat_char `,`.
                ELSE.
                  EXIT.
                ENDIF.
              ENDWHILE.

            ENDIF.

          ENDIF.

          eat_char `]`.

          IF mv_parse_to_data EQ abap_false.
            mr_json_builder->end( ). "end array
          ENDIF.

        WHEN `"`. " string
          IF data IS SUPPLIED AND mv_parse_to_data EQ abap_true.
            eat_string sdummy.
            " unescape string
            IF sdummy IS NOT INITIAL.
              REPLACE ALL OCCURRENCES OF `\"` IN sdummy WITH `"`.
              REPLACE ALL OCCURRENCES OF `\\` IN sdummy WITH `\`.
              type_descr = cl_abap_typedescr=>describe_by_data( data ).
              IF type_descr->kind EQ cl_abap_typedescr=>kind_elem.
                elem_descr ?= type_descr.
                CASE elem_descr->type_kind.
                  WHEN cl_abap_typedescr=>typekind_char.
                    IF elem_descr->output_length EQ 1 AND
                      Zcl_json_primitive=>c_boolean_types CS elem_descr->absolute_name.
                      IF sdummy(1) EQ `X` OR sdummy(1) EQ `t` OR sdummy(1) EQ `T` OR sdummy(1) EQ `x`.
                        data = abap_true.
                      ELSE.
                        data = abap_false.
                      ENDIF.
                      RETURN.
                    ENDIF.
                  WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
                    string_to_xstring( EXPORTING in = sdummy CHANGING out = data ).
                    RETURN.
                  WHEN cl_abap_typedescr=>typekind_date.
                    REPLACE FIRST OCCURRENCE OF REGEX `(\d{4})-(\d{2})-(\d{2})` IN sdummy WITH `$1$2$3`.
                  WHEN cl_abap_typedescr=>typekind_time.
                    REPLACE FIRST OCCURRENCE OF REGEX `(\d{2}):(\d{2}):(\d{2})` IN sdummy WITH `$1$2$3`.
                ENDCASE.
              ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                CREATE DATA lr_sdummy TYPE string.
                MOVE sdummy TO lr_sdummy->*.
                data ?= lr_sdummy.
                RETURN.
              ELSE.
                throw_error.
              ENDIF.
            ENDIF.
            MOVE sdummy TO data. " to avoid crashes due to data type inconsistency
          ELSE.
            eat_string sdummy.
            REPLACE ALL OCCURRENCES OF `\"` IN sdummy WITH `"`.
            REPLACE ALL OCCURRENCES OF `\\` IN sdummy WITH `\`.
            mr_json_builder->add( iv_name = name ir_element = Zcl_json_primitive=>create( sdummy ) ).
          ENDIF.
        WHEN `-`. " number
          IF data IS SUPPLIED AND mv_parse_to_data EQ abap_true.
            type_descr = cl_abap_typedescr=>describe_by_data( data ).
            IF type_descr->kind EQ type_descr->kind_ref AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
              CREATE DATA lr_idummy TYPE i.
              eat_number lr_idummy->*.
              data ?= lr_idummy.
            ELSEIF type_descr->kind EQ type_descr->kind_ref.
              throw_error.
            ELSE.
              eat_number data.
            ENDIF.
          ELSE.
            eat_number sdummy.
            mr_json_builder->add( iv_name = name ir_element = Zcl_json_primitive=>from_string( sdummy ) ).
          ENDIF.
        WHEN OTHERS.
          FIND FIRST OCCURRENCE OF mv_json+mv_offset(1) IN `0123456789`.
          IF sy-subrc IS INITIAL. " number
            IF data IS SUPPLIED AND mv_pritty_name_mode EQ abap_true.
              type_descr = cl_abap_typedescr=>describe_by_data( data ).
              IF type_descr->kind EQ type_descr->kind_ref AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                CREATE DATA lr_idummy TYPE i.
                eat_number lr_idummy->*.
                data ?= lr_idummy.
              ELSEIF type_descr->kind EQ type_descr->kind_ref.
                throw_error.
              ELSE.
                eat_number data.
              ENDIF.
            ELSE.
              eat_number sdummy.
              mr_json_builder->add( iv_name = name ir_element = Zcl_json_primitive=>from_string( sdummy ) ).
            ENDIF.
          ELSE. " true/false/null
            IF data IS SUPPLIED AND mv_pritty_name_mode EQ abap_true.
              type_descr = cl_abap_typedescr=>describe_by_data( data ).
              IF type_descr->kind EQ type_descr->kind_ref AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                CREATE DATA lr_bdummy TYPE abap_bool.
                eat_bool lr_bdummy->*.
                data ?= lr_bdummy.
              ELSEIF type_descr->kind EQ type_descr->kind_ref.
                throw_error.
              ELSE.
                eat_bool data.
              ENDIF.
            ELSE.
              eat_null bdummy.
              IF bdummy EQ abap_true.
                mr_json_builder->add( iv_name = name ir_element = Zcl_json_null=>gc_instance ).
              ELSE.
                eat_bool bdummy.
*                IF bdummy EQ '-'. "undefined
*                  Zcx_json_exception=>raise( iv_msg = 'Unexpected JSON values'  ).
*                ELSE.
                mr_json_builder->add( iv_name = name ir_element = Zcl_json_primitive=>create( iv_data = bdummy ) ).
*                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
      ENDCASE.
    CATCH Zcx_json_exception INTO lr_exjson.
      RAISE EXCEPTION lr_exjson.
    CATCH cx_root INTO lr_exroot.
      " JSON to ABAP parsing failed
      Zcx_json_exception=>raise( iv_msg_number = '006' previous = lr_exroot  ).
  ENDTRY.

ENDMETHOD.


METHOD string_to_xstring.
  DATA: lv_xstring TYPE xstring.

  CALL FUNCTION 'SSFC_BASE64_DECODE'
    EXPORTING
      b64data = in
    IMPORTING
      bindata = lv_xstring
    EXCEPTIONS
      OTHERS  = 1.

  IF sy-subrc IS INITIAL.
    MOVE lv_xstring TO out.
  ELSE.
    MOVE in TO out.
  ENDIF.
ENDMETHOD.


METHOD to_json.

  IF data IS SUPPLIED.
    DATA: lrf_descr TYPE REF TO cl_abap_typedescr.

    IF type_descr IS INITIAL.
      lrf_descr = cl_abap_typedescr=>describe_by_data( data ).
    ELSE.
      lrf_descr = type_descr.
    ENDIF.

    r_json = Zcl_json=>dump( data = data compress = compress pretty_name = pretty_name type_descr = lrf_descr ).

  ELSEIF json_element IS SUPPLIED AND json_element IS BOUND.

    r_json = json_element->to_string( ).

  ENDIF.
  " we do not do escaping of every single string value for white space characters,
  " but we do it on top, to replace multiple calls by 3 only, while we do not serialize
  " outlined/formatted JSON this shall not produce any harm
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN r_json WITH `\r\n`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN r_json WITH `\n`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN r_json WITH `\t`.
* REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed      IN r_json WITH `\f`.
* REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace      IN r_json WITH `\b`.

  IF name IS NOT INITIAL AND ( compress EQ abap_false OR r_json IS NOT INITIAL ).
    CONCATENATE `"` name `":` r_json INTO r_json.
  ENDIF.
ENDMETHOD.


METHOD xstring_to_string.
  DATA: lv_xstring TYPE xstring.

  " let us fix data conversion issues here
  lv_xstring = in.

  CALL FUNCTION 'SSFC_BASE64_ENCODE'
    EXPORTING
      bindata = lv_xstring
    IMPORTING
      b64data = out
    EXCEPTIONS
      OTHERS  = 1.

  IF sy-subrc IS NOT INITIAL.
    MOVE in TO out.
  ENDIF.
ENDMETHOD.
ENDCLASS.
