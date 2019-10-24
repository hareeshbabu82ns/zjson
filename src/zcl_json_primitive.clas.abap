CLASS zcl_json_primitive DEFINITION
  PUBLIC
  INHERITING FROM zcl_json_element
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .
    CLASS cl_abap_math DEFINITION LOAD .

    CLASS-DATA gt_premitives TYPE abap_component_tab READ-ONLY .
    CLASS-DATA gc_int2_max TYPE i VALUE 32767 ##NO_TEXT.
    CLASS-DATA gc_int4_max TYPE i VALUE cl_abap_math=>max_int4 ##NO_TEXT.
    CLASS-DATA c_boolean_types TYPE string VALUE `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL#\TYPE=BOOLEAN#\TYPE=BOOLE_D#\TYPE=XFELD` ##NO_TEXT.
    CLASS-DATA mv_regex_number TYPE REF TO cl_abap_regex READ-ONLY .
    CLASS-DATA mv_regex_json_utc_timestamp TYPE REF TO cl_abap_regex READ-ONLY .

    METHODS constructor
      IMPORTING
        !ir_value TYPE any .
    METHODS get_primitive_type
      RETURNING
        VALUE(rv_kind) TYPE int1 .
    METHODS set_value
      IMPORTING
        !ir_value TYPE any .
    CLASS-METHODS class_constructor .
    CLASS-METHODS create
      IMPORTING
        !iv_data            TYPE any
      RETURNING
        VALUE(rr_primitive) TYPE REF TO zcl_json_primitive .
    CLASS-METHODS from_string
      IMPORTING
        !iv_string          TYPE string
      RETURNING
        VALUE(rr_primitive) TYPE REF TO zcl_json_primitive .

    METHODS zif_json_element~as_data
        REDEFINITION .
    METHODS zif_json_element~as_string
        REDEFINITION .
    METHODS zif_json_element~deep_copy
        REDEFINITION .
    METHODS zif_json_element~equals
        REDEFINITION .
    METHODS zif_json_element~get_abap_type
        REDEFINITION .
    METHODS zif_json_element~get_type
        REDEFINITION .
    METHODS zif_json_element~is_boolean
        REDEFINITION .
    METHODS zif_json_element~is_number
        REDEFINITION .
    METHODS zif_json_element~is_string
        REDEFINITION .
    METHODS zif_json_element~to_dref
        REDEFINITION .
    METHODS zif_json_element~to_string
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mr_value TYPE REF TO data .
    DATA mr_descr TYPE REF TO cl_abap_elemdescr .

    CLASS-METHODS prepare_premitive_descriptors .
ENDCLASS.



CLASS zcl_json_primitive IMPLEMENTATION.


  METHOD class_constructor.
    prepare_premitive_descriptors( ).

    CREATE OBJECT mv_regex_number
      EXPORTING
        pattern = '^-?\d+(\.\d*)?-?$'.
*      pattern = '^([\d\?\*]+([\.][\d\?\*]*|,[\d\?\*]+)?([eE][+-]?[\d\?\*]+)?).*'. "#EC NOTEXT

    CREATE OBJECT mv_regex_json_utc_timestamp
      EXPORTING
*       pattern     = '^\d{4})-(\d{1,2})-(\d{1,2}?)[Tt](\d{1,2}):(\d{1,2}):(\d{1,2})([\.,]\d{1,6})?[Zz]?$'.
*       pattern     = '^\d{4}-\d{1,2}-\d{1,2}?T\d{1,2}:\d{1,2}:\d{1,2}[\.,]\d{1,6}?Z?$'
        pattern     = '^\d{4}-\d{1,2}-\d{1,2}T\d{1,2}:\d{1,2}:\d{1,2}([,.]\d{1,6})?Z$'
*       pattern     = '^[0-9tz.,:-]+$'
        ignore_case = abap_true.


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
      zcx_json_exception=>raise( iv_msg_number = '007' ).
    ENDIF.

*  CL_ABAP_EXCEPTIONAL_VALUES=>get_max_value( data ).

    DATA: lv_len           TYPE i,
          lv_split_whole   TYPE string,
          lv_split_decimal TYPE string.

    DATA: lv_int       TYPE i,
          lr_descr     TYPE REF TO cl_abap_elemdescr,
          lr_data      TYPE REF TO data,
          lv_timestamp TYPE timestamp.

    FIELD-SYMBOLS: <fv_data> TYPE any.

    IF iv_string EQ 'null' OR iv_string EQ 'NULL'. "null
      "TBD - should not come here fr NULL creation
    ELSEIF iv_string EQ 'true' OR iv_string EQ 'TRUE'. "boolean true
      rr_primitive = zcl_json_primitive=>create( abap_true ).
    ELSEIF iv_string EQ 'false' OR iv_string EQ 'FALSE'. "boolean false
      rr_primitive = zcl_json_primitive=>create( abap_false ).
    ELSEIF iv_string CO '0123456789-+eE. '. "number
      IF iv_string NA '.eE'.
        TRY.
            MOVE iv_string TO lv_int.
            rr_primitive = zcl_json_primitive=>create( lv_int ).
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
        rr_primitive = zcl_json_primitive=>create( <fv_data> ).
      ENDIF.

    ELSE.

      IF iv_string CO '0123456789TZ.,-:'. "timestamp

        lv_timestamp = zcl_json_util=>convert_json_utc_atimestamp( iv_string ).
        IF lv_timestamp IS NOT INITIAL.
          rr_primitive = zcl_json_primitive=>create( lv_timestamp ).
        ENDIF.

      ELSE.
*        rr_primitive = zcl_json_primitive=>create( iv_string ).
      ENDIF.

    ENDIF.

    IF rr_primitive IS NOT BOUND.
      rr_primitive = zcl_json_primitive=>create( iv_string ).
    ENDIF.

  ENDMETHOD.


  METHOD get_primitive_type.

    DATA: lv_type_name  TYPE string,
          lv_field_name TYPE fieldname.

    IF mr_descr->is_ddic_type( ) = abap_true.
      data(ls_field) = mr_descr->get_ddic_field( ).
    ENDIF.

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
        cl_abap_typedescr=>typekind_int1 OR
        cl_abap_typedescr=>typekind_int8 OR
        cl_abap_typedescr=>typekind_int2 OR
        cl_abap_typedescr=>typekind_numeric OR
        cl_abap_typedescr=>typekind_num.

        rv_kind = c_type_primitive_number.

      WHEN cl_abap_typedescr=>typekind_char.

        IF mr_descr->absolute_name CS 'ABAP_BOOL' OR
          mr_descr->absolute_name CS 'BOOLEAN' OR
          ls_field-domname CS 'BOOLEAN'.
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
          zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Primitive' ).
        ENDIF.

*      CREATE DATA mr_value TYPE (mr_descr->type_kind).
        CREATE DATA mr_value TYPE HANDLE mr_descr.

        ASSIGN mr_value->* TO <fr_value>.

        MOVE ir_value TO <fr_value>.

      CATCH cx_root INTO lr_cxroot.
        " Not a Valid Primitive Type
        zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Primitive' previous = lr_cxroot ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_json_element~as_data.

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

        zcl_json_parser=>string_to_xstring( EXPORTING in = <fv_value>
                                                  CHANGING out = cv_data ).
      WHEN cl_abap_typedescr=>typekind_char.
        IF ( ( mr_descr->output_length EQ 32 OR mr_descr->output_length EQ 16 )
                AND mr_descr->absolute_name CS '_GUID' ) OR
          ( ( is_comp_attrs-type_kind EQ cl_abap_typedescr=>typekind_char
                AND is_comp_attrs-length EQ 64 ) AND is_comp_attrs-name CS '_GUID' ).
          zcl_json_parser=>string_to_xstring( EXPORTING in = <fv_value>
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


  METHOD zif_json_element~as_string.
    FIELD-SYMBOLS: <fv_value> TYPE any.
    ASSIGN mr_value->* TO <fv_value>.
    rv_string = |{ <fv_value> }|.
  ENDMETHOD.


  METHOD zif_json_element~deep_copy.
*  rv_element = me.
    FIELD-SYMBOLS: <fs_val> TYPE any.

    ASSIGN mr_value->* TO <fs_val>.

    rv_element ?= zcl_json_primitive=>create( <fs_val> ).


  ENDMETHOD.


  METHOD zif_json_element~equals.
    rv_equal = abap_false.
    IF ir_element IS BOUND.
      CHECK get_type( ) EQ ir_element->get_type( ).

      DATA: lr_typedescr TYPE REF TO cl_abap_typedescr.
      lr_typedescr ?= ir_element->get_abap_type( ).

      CHECK mr_descr->type_kind EQ lr_typedescr->type_kind.

      DATA: lr_dref TYPE REF TO data.
      FIELD-SYMBOLS: <fs_val_src>  TYPE any,
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


  METHOD zif_json_element~get_abap_type.
    rr_type ?= mr_descr.
  ENDMETHOD.


  METHOD zif_json_element~get_type.
    rv_type = zif_json_element=>c_type_primitive.
  ENDMETHOD.


  METHOD zif_json_element~is_boolean.
    IF get_primitive_type( ) EQ c_type_primitive_boolean.
      iv_return = abap_true.
    ELSE.
      iv_return = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD zif_json_element~is_number.
    IF get_primitive_type( ) EQ c_type_primitive_number.
      iv_return = abap_true.
    ELSE.
      iv_return = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD zif_json_element~is_string.
    IF get_primitive_type( ) EQ c_type_primitive_string.
      iv_return = abap_true.
    ELSE.
      iv_return = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD zif_json_element~to_dref.
    rr_dref = mr_value.
  ENDMETHOD.


  METHOD zif_json_element~to_string.
    FIELD-SYMBOLS: <fr_val> TYPE any.
    DATA: lv_str   TYPE string.

    IF mr_descr->is_ddic_type( ) = abap_true.
      data(ls_field) = mr_descr->get_ddic_field( ).
    ENDIF.

    rv_string = zif_json_element=>c_type_null.

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
          rv_string = zcl_json_parser=>xstring_to_string( <fr_val> ).
          escape_json_inplace rv_string.
          CONCATENATE `"` rv_string `"` INTO rv_string.
        ENDIF.
      WHEN cl_abap_typedescr=>typekind_char.
        IF mr_descr->output_length EQ 1 AND
            ( c_boolean_types CS mr_descr->absolute_name OR
               c_boolean_types CS  ls_field-domname ).
          IF <fr_val> EQ abap_true.
            rv_string = `true`.                             "#EC NOTEXT
          ELSEIF <fr_val> EQ abap_false.
            rv_string = `false`.                            "#EC NOTEXT
          ELSE.
            rv_string = `null`.                             "#EC NOTEXT
          ENDIF.
        ELSEIF ( mr_descr->output_length EQ 32 OR mr_descr->output_length EQ 16 )
          AND mr_descr->absolute_name CS '_GUID'.
          rv_string = `"` && zcl_json_parser=>xstring_to_string( <fr_val> ) && `"`.
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
          rv_string = `null`.                               "#EC NOTEXT
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
