class ZCL_JSON_PRIMITIVE definition
  public
  inheriting from ZCL_JSON_ELEMENT
  final
  create public .

public section.
  type-pools ABAP .
  class CL_ABAP_MATH definition load .

  class-data GT_PREMITIVES type ABAP_COMPONENT_TAB read-only .
  class-data GC_INT2_MAX type I value 32767 ##NO_TEXT.
  class-data GC_INT4_MAX type I value CL_ABAP_MATH=>MAX_INT4 ##NO_TEXT.
  class-data C_BOOLEAN_TYPES type STRING value `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL#\TYPE=BOOLEAN#\TYPE=BOOLE_D#\TYPE=XFELD` ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !IR_VALUE type ANY .
  methods GET_PRIMITIVE_TYPE
    returning
      value(RV_KIND) type INT1 .
  methods SET_VALUE
    importing
      !IR_VALUE type ANY .
  class-methods CLASS_CONSTRUCTOR .
  class-methods CREATE
    importing
      !IV_DATA type ANY
    returning
      value(RR_PRIMITIVE) type ref to ZCL_JSON_PRIMITIVE .
  class-methods FROM_STRING
    importing
      !IV_STRING type STRING
    returning
      value(RR_PRIMITIVE) type ref to ZCL_JSON_PRIMITIVE .

  methods ZIF_JSON_ELEMENT~AS_DATA
    redefinition .
  methods ZIF_JSON_ELEMENT~AS_STRING
    redefinition .
  methods ZIF_JSON_ELEMENT~DEEP_COPY
    redefinition .
  methods ZIF_JSON_ELEMENT~EQUALS
    redefinition .
  methods ZIF_JSON_ELEMENT~GET_ABAP_TYPE
    redefinition .
  methods ZIF_JSON_ELEMENT~GET_TYPE
    redefinition .
  methods ZIF_JSON_ELEMENT~IS_BOOLEAN
    redefinition .
  methods ZIF_JSON_ELEMENT~IS_NUMBER
    redefinition .
  methods ZIF_JSON_ELEMENT~IS_STRING
    redefinition .
  methods ZIF_JSON_ELEMENT~TO_DREF
    redefinition .
  methods ZIF_JSON_ELEMENT~TO_STRING
    redefinition .
protected section.
private section.

  data MR_VALUE type ref to DATA .
  data MR_DESCR type ref to CL_ABAP_ELEMDESCR .
  class-data MV_REGEX_NUMBER type ref to CL_ABAP_REGEX .

  class-methods PREPARE_PREMITIVE_DESCRIPTORS .
ENDCLASS.



CLASS ZCL_JSON_PRIMITIVE IMPLEMENTATION.


METHOD class_constructor.
  prepare_premitive_descriptors( ).

  CREATE OBJECT mv_regex_number
    EXPORTING
      pattern = '^-?\d+(\.\d*)?-?$'.
*      pattern = '^([\d\?\*]+([\.][\d\?\*]*|,[\d\?\*]+)?([eE][+-]?[\d\?\*]+)?).*'. "#EC NOTEXT

ENDMETHOD.


METHOD constructor.
  super->constructor( ).

  set_value( ir_value ).

ENDMETHOD.


METHOD create.
  CREATE OBJECT rr_primitive
    EXPORTING
      ir_value = iv_data.
ENDMETHOD.


METHOD from_string.
  IF iv_string IS INITIAL.
    " Parameter Initial
    Zcx_json_exception=>raise( iv_msg_number = '007' ).
  ENDIF.

*  CL_ABAP_EXCEPTIONAL_VALUES=>get_max_value( data ).

  DATA: lv_len TYPE i,
        lv_split_whole TYPE string,
        lv_split_decimal TYPE string.

  DATA: lv_int TYPE i,
        lr_descr TYPE REF TO cl_abap_elemdescr,
        lr_data TYPE REF TO data.

  FIELD-SYMBOLS: <fv_data> TYPE any.

  IF iv_string EQ 'null' OR iv_string EQ 'NULL'. "null
    "TBD - should not come here fr NULL creation
  ELSEIF iv_string EQ 'true' OR iv_string EQ 'TRUE'. "boolean true
    rr_primitive = Zcl_json_primitive=>create( abap_true ).
  ELSEIF iv_string EQ 'false' OR iv_string EQ 'FALSE'. "boolean false
    rr_primitive = Zcl_json_primitive=>create( abap_false ).
  ELSEIF iv_string CO '0123456789-+eE. '. "number
    IF iv_string NA '.eE'.
      TRY.
          MOVE iv_string TO lv_int.
          rr_primitive = Zcl_json_primitive=>create( lv_int ).
        CATCH cx_root.
          "failed to convert to int, must be float/packed
      ENDTRY.
    ENDIF.
    "packed
    IF rr_primitive IS NOT BOUND. "not yet converted
      SPLIT iv_string AT '.' INTO lv_split_whole lv_split_decimal.
      lv_len = strlen( lv_split_decimal ). "fractional part
      lv_int = strlen( lv_split_whole ). "intiger part
      lr_descr = cl_abap_elemdescr=>get_p( p_length = lv_int p_decimals = lv_len ).
      CREATE DATA lr_data TYPE HANDLE lr_descr.
      ASSIGN lr_data->* TO <fv_data>.
      MOVE iv_string TO <fv_data>.
      rr_primitive = Zcl_json_primitive=>create( <fv_data> ).
    ENDIF.
  ELSE.
    rr_primitive = Zcl_json_primitive=>create( iv_string ).
  ENDIF.


ENDMETHOD.


METHOD get_primitive_type.

  CASE mr_descr->type_kind.

    WHEN cl_abap_typedescr=>typekind_string OR
      cl_abap_typedescr=>typekind_clike OR
      cl_abap_typedescr=>typekind_csequence OR
      cl_abap_typedescr=>typekind_date OR
      cl_abap_typedescr=>typekind_time OR
      cl_abap_typedescr=>typekind_xstring OR
      cl_abap_typedescr=>typekind_hex.

      rv_kind = c_type_primitive_string.

    WHEN cl_abap_typedescr=>typekind_packed OR
      cl_abap_typedescr=>typekind_float OR
      cl_abap_typedescr=>typekind_int OR
      cl_abap_typedescr=>typekind_numeric OR
      cl_abap_typedescr=>typekind_num.

      rv_kind = c_type_primitive_number.

    WHEN cl_abap_typedescr=>typekind_char.

      IF mr_descr->absolute_name CS 'ABAP_BOOL' OR
        mr_descr->absolute_name CS 'BOOLEAN'.
        rv_kind = c_type_primitive_boolean.
      ELSE.
        rv_kind = c_type_primitive_string.
      ENDIF.

    WHEN OTHERS.
      rv_kind = c_type_null.

  ENDCASE.

ENDMETHOD.


METHOD prepare_premitive_descriptors.

  DATA: ls_comp LIKE LINE OF gt_premitives.

  TRY.

      ls_comp-name = 'GUID'.
      ls_comp-type = cl_abap_elemdescr=>get_c( 32 ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'DATE'.
      ls_comp-type = cl_abap_elemdescr=>get_d( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'TIME'.
      ls_comp-type = cl_abap_elemdescr=>get_t( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'TIMESTAMP'.
      ls_comp-type ?= cl_abap_elemdescr=>describe_by_name( 'TZNTSTMPS' ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'TIMESTAMPL'.
      ls_comp-type ?= cl_abap_elemdescr=>describe_by_name( 'TZNTSTMPL' ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'TINYINT'.
      ls_comp-type = cl_abap_elemdescr=>get_i( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'SMALLINT'.
      ls_comp-type = cl_abap_elemdescr=>get_i( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'INTEGER'.
      ls_comp-type = cl_abap_elemdescr=>get_i( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'BIGINT'.
      ls_comp-type = cl_abap_elemdescr=>get_p( p_length = 12  p_decimals = 0 ).
      APPEND ls_comp TO gt_premitives.

*      ls_comp-name = 'DECIMAL'.
*      ls_comp-name = 'DEC'.
*      ls_comp-name = 'SMALLDECIMAL'.
*      ls_comp-type = cl_abap_elemdescr=>get_p( p_length = <ls_col>-data_length  p_decimals = <ls_col>-data_precision ).

      ls_comp-name = 'REAL'.
      ls_comp-type = cl_abap_elemdescr=>get_f( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'DOUBLE'.
      ls_comp-type = cl_abap_elemdescr=>get_f( ).
      APPEND ls_comp TO gt_premitives.


      ls_comp-name = 'CHAR'.
      ls_comp-type = cl_abap_elemdescr=>get_c( 1 ).
      APPEND ls_comp TO gt_premitives.


      ls_comp-name = 'ALPHANUM'.
      ls_comp-type = cl_abap_elemdescr=>get_n( 10 ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'NUMBER'.
      ls_comp-type = cl_abap_elemdescr=>get_n( 10 ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'VARBINARY'.
      ls_comp-type = cl_abap_elemdescr=>get_xstring( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'BLOB'.
      ls_comp-type = cl_abap_elemdescr=>get_xstring( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'CLOB'.
      ls_comp-type = cl_abap_elemdescr=>get_string( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'NCLOB'.
      ls_comp-type = cl_abap_elemdescr=>get_string( ).
      APPEND ls_comp TO gt_premitives.

      ls_comp-name = 'TEXT'.
      ls_comp-type = cl_abap_elemdescr=>get_string( ).
      APPEND ls_comp TO gt_premitives.

    CATCH cx_root.
  ENDTRY.

ENDMETHOD.


METHOD set_value.

  FIELD-SYMBOLS: <fr_value> TYPE any.
  DATA: lr_cxroot TYPE REF TO cx_root.

  TRY .
      mr_descr ?= cl_abap_elemdescr=>describe_by_data( ir_value ).

      IF mr_descr->kind NE cl_abap_typedescr=>kind_elem
        OR get_primitive_type( ) EQ c_type_null.
        " Not a Valid Primitive Type
        Zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Primitive' ).
      ENDIF.

*      CREATE DATA mr_value TYPE (mr_descr->type_kind).
      CREATE DATA mr_value TYPE HANDLE mr_descr.

      ASSIGN mr_value->* TO <fr_value>.

      MOVE ir_value TO <fr_value>.

    CATCH cx_root INTO lr_cxroot.
      " Not a Valid Primitive Type
      Zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Primitive' previous = lr_cxroot ).
  ENDTRY.

ENDMETHOD.


METHOD Zif_json_element~as_data.

  DATA: lv_line      TYPE text255.

  FIELD-SYMBOLS: <fv_value> TYPE any.
  ASSIGN mr_value->* TO <fv_value>.

  IF mr_descr->type_kind EQ is_comp_attrs-type_kind.
    cv_data = <fv_value>.
    RETURN.
  ENDIF.

  CASE is_comp_attrs-type_kind.

    WHEN cl_abap_elemdescr=>typekind_date.
      "TODO: review cl_abap_datfm=>conv_date_ext_to_int
      cv_data = |{ <fv_value>(4) }{ <fv_value>+5(2) }{ <fv_value>+8(2) }|.

    WHEN cl_abap_typedescr=>typekind_time.
      cv_data = |{ <fv_value>(2) }{ <fv_value>+3(2) }{ <fv_value>+6(2) }|.

    WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.

      Zcl_json_parser=>string_to_xstring( EXPORTING in = <fv_value>
                                                CHANGING out = cv_data ).
    WHEN cl_abap_typedescr=>typekind_char.
      IF ( ( mr_descr->output_length EQ 32 OR mr_descr->output_length EQ 16 )
              AND mr_descr->absolute_name CS '_GUID' ) OR
        ( ( is_comp_attrs-type_kind EQ cl_abap_typedescr=>typekind_char
              AND is_comp_attrs-length EQ 64 ) AND is_comp_attrs-name CS '_GUID' ).
        Zcl_json_parser=>string_to_xstring( EXPORTING in = <fv_value>
                                                  CHANGING out = cv_data ).
      ELSE.
        cv_data = <fv_value>.
      ENDIF.

    WHEN OTHERS.
*      IF is_comp_attrs IS NOT INITIAL.
*        lv_line = <fv_value>.
*        CALL FUNCTION 'RS_CONV_EX_2_IN_NO_DD'
*          EXPORTING
*            input_external  = lv_line(is_comp_attrs-length)
*          IMPORTING
*            output_internal = cv_data
*          EXCEPTIONS
*            OTHERS          = 20.
*      ENDIF.

  ENDCASE.

*  "copied from CL_CRM_BOL_ABSTR_BO=>CONVERT_FROM_STRING
*
*  DATA: lv_dtel      TYPE rollname,
*         lv_typedescr TYPE REF TO cl_abap_elemdescr.
*
**     create new value
*  lv_typedescr ?= cl_abap_typedescr=>describe_by_data( cv_data ).
*  CASE lv_typedescr->type_kind.
*
*    WHEN cl_abap_typedescr=>typekind_string OR
*        cl_abap_typedescr=>typekind_csequence OR
*        cl_abap_typedescr=>typekind_clike.
**       no conversion necessary
*
*      cv_data = <fv_value>.
*
*    WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
*
*      Zcl_json_parser=>string_to_xstring( EXPORTING in = <fv_value>
*                                                CHANGING out = cv_data ).
*
*    WHEN OTHERS.
**   convert input to new value
*      lv_line = <fv_value>.
*      lv_dtel = lv_typedescr->get_relative_name( ).
*      " ... when underlying DDIC type is based on a data element
*      IF lv_dtel IS NOT INITIAL AND lv_typedescr->is_ddic_type( ) = abap_true.
*        CALL FUNCTION 'RS_CONV_EX_2_IN_DTEL'
*          EXPORTING
*            input_external              = lv_line(lv_typedescr->output_length)
*            dtel                        = lv_dtel
*          IMPORTING
*            output_internal             = cv_data
*          EXCEPTIONS
*            input_not_numerical         = 1
*            too_many_decimals           = 2
*            more_than_one_sign          = 3
*            ill_thousand_separator_dist = 4
*            too_many_digits             = 5
*            sign_for_unsigned           = 6
*            too_large                   = 7
*            too_small                   = 8
*            invalid_date_format         = 9
*            invalid_date                = 10
*            invalid_time_format         = 11
*            invalid_time                = 12
*            invalid_hex_digit           = 13
*            unexpected_error            = 14
*            input_too_long              = 15
*            no_decimals                 = 16
*            invalid_float               = 17
*            conversion_exit_error       = 18
*            OTHERS                      = 19.
*        " ... when input is an anonymous DDIC type etc.
*      ELSE.
*        CALL FUNCTION 'RS_CONV_EX_2_IN_NO_DD'
*          EXPORTING
*            input_external              = lv_line(lv_typedescr->output_length)
*          IMPORTING
*            output_internal             = cv_data
*          EXCEPTIONS
*            input_not_numerical         = 1
*            too_many_decimals           = 2
*            more_than_one_sign          = 3
*            ill_thousand_separator_dist = 4
*            too_many_digits             = 5
*            sign_for_unsigned           = 6
*            too_large                   = 7
*            too_small                   = 8
*            invalid_date_format         = 9
*            invalid_date                = 10
*            invalid_time_format         = 11
*            invalid_time                = 12
*            invalid_hex_digit           = 13
*            unexpected_error            = 14
*            input_too_long              = 15
*            no_decimals                 = 16
*            invalid_float               = 17
*            illegal_type                = 18
*            conversion_exit_error       = 19
*            OTHERS                      = 20.
*      ENDIF.
*      IF sy-subrc <> 0.
*        CALL FUNCTION 'BAL_DSP_TXT_MSG_READ'
*          EXPORTING
*            i_langu        = sy-langu
*            i_msgid        = sy-msgid
*            i_msgno        = sy-msgno
*            i_msgv1        = sy-msgv1
*            i_msgv2        = sy-msgv2
*            i_msgv3        = sy-msgv3
*            i_msgv4        = sy-msgv4
*          IMPORTING
*            e_message_text = lv_line.
*        RAISE EXCEPTION TYPE cx_crm_genil_conversion_error
*          EXPORTING
*            text = lv_line.
*      ENDIF.
*  ENDCASE.



  IF cv_data IS INITIAL.
    cv_data = <fv_value>.
  ENDIF.

ENDMETHOD.


METHOD Zif_json_element~as_string.
  FIELD-SYMBOLS: <fv_value> TYPE any.
  ASSIGN mr_value->* TO <fv_value>.
  rv_string = |{ <fv_value> }|.
ENDMETHOD.


METHOD Zif_json_element~deep_copy.
*  rv_element = me.
  FIELD-SYMBOLS: <fs_val> TYPE any.

  ASSIGN mr_value->* TO <fs_val>.

  rv_element ?= Zcl_json_primitive=>create( <fs_val> ).


ENDMETHOD.


METHOD Zif_json_element~equals.
  rv_equal = abap_false.
  IF ir_element IS BOUND.
    CHECK get_type( ) EQ ir_element->get_type( ).

    DATA: lr_typedescr TYPE REF TO cl_abap_typedescr.
    lr_typedescr ?= ir_element->get_abap_type( ).

    CHECK mr_descr->type_kind EQ lr_typedescr->type_kind.

    DATA: lr_dref TYPE REF TO data.
    FIELD-SYMBOLS: <fs_val_src> TYPE any,
                   <fs_val_dest> TYPE any.

    lr_dref = ir_element->to_dref( ).
    ASSIGN lr_dref->* TO <fs_val_dest>.
    ASSIGN mr_value->* TO <fs_val_src>.

    CHECK <fs_val_src> EQ <fs_val_dest>.

  ELSEIF ir_data IS BOUND.
    RETURN. "not null
  ENDIF.
  rv_equal = abap_true.
ENDMETHOD.


METHOD Zif_json_element~get_abap_type.
  rr_type ?= mr_descr.
ENDMETHOD.


METHOD Zif_json_element~get_type.
  rv_type = Zif_json_element=>c_type_primitive.
ENDMETHOD.


METHOD Zif_json_element~is_boolean.
  IF get_primitive_type( ) EQ c_type_primitive_boolean.
    iv_return = abap_true.
  ELSE.
    iv_return = abap_false.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_element~is_number.
  IF get_primitive_type( ) EQ c_type_primitive_number.
    iv_return = abap_true.
  ELSE.
    iv_return = abap_false.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_element~is_string.
  IF get_primitive_type( ) EQ c_type_primitive_string.
    iv_return = abap_true.
  ELSE.
    iv_return = abap_false.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_element~to_dref.
  rr_dref = mr_value.
ENDMETHOD.


METHOD Zif_json_element~to_string.
  FIELD-SYMBOLS: <fr_val> TYPE any.
  DATA: lv_str TYPE string.

  rv_string = Zif_json_element=>c_type_null.

  IF mr_value IS NOT BOUND.
    IF ir_stream IS BOUND.
      ir_stream->write( rv_string ).
    ENDIF.
    RETURN.
  ENDIF.

  ASSIGN mr_value->* TO <fr_val>.

  "TODO: need to write conversion for different primitives


  CASE mr_descr->type_kind.
    WHEN cl_abap_typedescr=>typekind_float OR cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1 OR
         cl_abap_typedescr=>typekind_int2 OR cl_abap_typedescr=>typekind_packed OR `8`. " TYPEKIND_INT8 -> '8' only from 7.40.
      IF <fr_val> IS INITIAL.
        rv_string = `0`.
      ELSE.
        MOVE <fr_val> TO rv_string.
        IF <fr_val> LT 0.
          SHIFT rv_string RIGHT CIRCULAR.
        ELSE.
          CONDENSE rv_string.
        ENDIF.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_num.
      IF <fr_val> IS INITIAL.
        rv_string = `0`.
      ELSE.
        MOVE <fr_val> TO rv_string.
        SHIFT rv_string LEFT DELETING LEADING ` 0`.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_string OR cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_clike.
      IF <fr_val> IS INITIAL.
        rv_string = `""`.
      ELSE.
        escape_json <fr_val> rv_string.
        CONCATENATE `"` rv_string `"` INTO rv_string.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
      IF <fr_val> IS INITIAL.
        rv_string = `""`.
      ELSE.
        rv_string = Zcl_json_parser=>xstring_to_string( <fr_val> ).
        escape_json_inplace rv_string.
        CONCATENATE `"` rv_string `"` INTO rv_string.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_char.
      IF mr_descr->output_length EQ 1 AND c_boolean_types CS mr_descr->absolute_name.
        IF <fr_val> EQ abap_true.
          rv_string = `true`.                               "#EC NOTEXT
        ELSE.
          rv_string = `false`.                              "#EC NOTEXT
        ENDIF.
      ELSEIF ( mr_descr->output_length EQ 32 OR mr_descr->output_length EQ 16 )
        AND mr_descr->absolute_name CS '_GUID'.
        rv_string = `"` && Zcl_json_parser=>xstring_to_string( <fr_val> ) && `"`.
      ELSE.
        escape_json <fr_val> rv_string.
        CONCATENATE `"` rv_string `"` INTO rv_string.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_date.
      CONCATENATE `"` <fr_val>(4) `-` <fr_val>+4(2) `-` <fr_val>+6(2) `"` INTO rv_string.
    WHEN cl_abap_typedescr=>typekind_time.
      CONCATENATE `"` <fr_val>(2) `:` <fr_val>+2(2) `:` <fr_val>+4(2) `"` INTO rv_string.
    WHEN OTHERS.
      IF <fr_val> IS INITIAL.
        rv_string = `null`.                                 "#EC NOTEXT
      ELSE.
        MOVE <fr_val> TO rv_string.
      ENDIF.
  ENDCASE.

*  CASE mr_descr->type_kind.
*
*    WHEN cl_abap_typedescr=>typekind_date.
*      rv_string = |"{ <fr_val>(4) } - { <fr_val>+4(2) } - { <fr_val>+6(2) } "|.
*
*    WHEN cl_abap_typedescr=>typekind_time.
*      rv_string = |"{ <fr_val>(2) } : { <fr_val>+2(2) } : { <fr_val>+4(2) } "|.
*
*    WHEN
*    cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
*      "TODO: convert to string
*
*    WHEN cl_abap_typedescr=>typekind_packed OR
*      cl_abap_typedescr=>typekind_float OR
*      cl_abap_typedescr=>typekind_int OR
*      cl_abap_typedescr=>typekind_int1 OR
*      cl_abap_typedescr=>typekind_int2 OR
*      `8` OR "cl_abap_typedescr=>typekind_int8 OR "only from 7.40
*      cl_abap_typedescr=>typekind_numeric OR
*      cl_abap_typedescr=>typekind_num.
*
**      MOVE <fr_val> TO rv_string.
**      IF <fr_val> LT 0.
**        SHIFT rv_string RIGHT CIRCULAR BY 1 PLACES.
**      ENDIF.
*      rv_string = |{ <fr_val> }|.
*
*    WHEN cl_abap_typedescr=>typekind_num.
*      MOVE <fr_val> TO rv_string.
*      SHIFT rv_string LEFT DELETING LEADING ` 0`.
*
*    WHEN cl_abap_typedescr=>typekind_string OR
*          cl_abap_typedescr=>typekind_csequence OR
*          cl_abap_typedescr=>typekind_clike.
*      IF <fr_val> IS INITIAL.
*        rv_string = `""`.
*      ELSE.
*        escape_json <fr_val> rv_string.
*        CONCATENATE `"` rv_string `"` INTO rv_string.
*      ENDIF.
*
*    WHEN cl_abap_typedescr=>typekind_char.
*
*      IF mr_descr->output_length EQ 1 AND
*        c_boolean_types CS mr_descr->absolute_name.
*        IF <fr_val> EQ abap_true.
*          rv_string = Zif_json_element=>c_value_bool_true.
*        ELSE.
*          rv_string = Zif_json_element=>c_value_bool_false.
*        ENDIF.
*      ELSE.
**        MOVE <fr_val> TO rv_string.
**        CONCATENATE `"` rv_string `"` INTO rv_string.
*        escape_json <fr_val> rv_string.
*        CONCATENATE `"` rv_string `"` INTO rv_string.
*      ENDIF.
*
*    WHEN OTHERS.
**      MOVE <fr_val> TO rv_string.
**      CONCATENATE `"` rv_string `"` INTO rv_string.
*      rv_string = |"{ <fr_val> }"|.
*
*  ENDCASE.
  IF ir_stream IS BOUND.
    ir_stream->write( rv_string ).
  ENDIF.

ENDMETHOD.
ENDCLASS.
