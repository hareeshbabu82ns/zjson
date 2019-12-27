class ZCL_JSON_OBJECT definition
  public
  inheriting from ZCL_JSON_ELEMENT
  final
  create public .

public section.
  type-pools ABAP .

  data MT_MEMBERS type ZJSON_NAME_ELEMENT_TAB read-only .

  methods ADD
    importing
      !IV_NAME type STRING
      !IR_ELEMENT type ref to ZIF_JSON_ELEMENT optional
      !IV_VALUE type ANY optional .
  methods REMOVE
    importing
      !IV_NAME type STRING
    returning
      value(RR_ELEMENT) type ref to ZIF_JSON_ELEMENT .
  methods HAS
    importing
      !IV_NAME type STRING
    returning
      value(RV_EXIST) type ABAP_BOOL .
  methods GET
    importing
      !IV_NAME type STRING
    returning
      value(RR_ELEMENT) type ref to ZIF_JSON_ELEMENT .
  methods CONSTRUCTOR
    importing
      value(IS_DATA) type ANY optional
      value(IT_NAMES_MAP) type ZJSON_NAME_VALUE_TAB optional
      value(IV_MAP_IGNORE_REST) type ABAP_BOOL default ABAP_FALSE .

  methods ZIF_JSON_ELEMENT~AS_DATA
    redefinition .
  methods ZIF_JSON_ELEMENT~DEEP_COPY
    redefinition .
  methods ZIF_JSON_ELEMENT~EQUALS
    redefinition .
  methods ZIF_JSON_ELEMENT~GET_ABAP_TYPE
    redefinition .
  methods ZIF_JSON_ELEMENT~GET_TYPE
    redefinition .
  methods ZIF_JSON_ELEMENT~IS_OBJECT
    redefinition .
  methods ZIF_JSON_ELEMENT~TO_DREF
    redefinition .
  methods ZIF_JSON_ELEMENT~TO_STRING
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_BY_INDEX
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_CURRENT
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_FIRST
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_LAST
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_NEXT
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_PREVIOUS
    redefinition .
  methods ZIF_JSON_ITERATOR~SIZE
    redefinition .
  methods ZIF_JSON_ELEMENT~AS_STRING
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_JSON_OBJECT IMPLEMENTATION.


METHOD add.
  DATA: ls_member TYPE Zjson_name_element_str,
        lr_attr TYPE REF TO Zcl_json_object_attr.

  ls_member-name = iv_name.

  IF ir_element IS NOT BOUND.
    IF iv_value IS SUPPLIED.
      CREATE OBJECT ls_member-value TYPE Zcl_json_primitive
        EXPORTING
          ir_value = iv_value.
    ELSE.
      ls_member-value = Zcl_json_null=>get_instance( ).
    ENDIF.
  ELSEIF ir_element->is_object_attribute( ) EQ abap_true.
    lr_attr ?= ir_element.
    ls_member-name = lr_attr->name.
    ls_member-value = lr_attr->value.
  ELSE.
    ls_member-value = ir_element.
  ENDIF.


  APPEND ls_member TO mt_members.

ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    CHECK is_data IS NOT INITIAL.

    DATA:
      lr_structdesc TYPE REF TO cl_abap_structdescr,
      lr_cxroot     TYPE REF TO cx_root.

    DATA:
      lt_fields TYPE abap_component_tab,
      lv_str    TYPE string.

    FIELD-SYMBOLS:
      <fv_value>    TYPE any,
      <fs_name_map> TYPE zjson_name_value_str,
      <fs_field>    TYPE abap_componentdescr.


    TRY .
        lr_structdesc ?= cl_abap_structdescr=>describe_by_data( is_data ).

        lt_fields = lr_structdesc->get_components( ).

        LOOP AT lt_fields ASSIGNING <fs_field>.
          ASSIGN COMPONENT <fs_field>-name OF STRUCTURE is_data TO <fv_value>.
          CHECK sy-subrc IS INITIAL.

          "find the name map to json name
          READ TABLE it_names_map WITH KEY name = <fs_field>-name
              ASSIGNING <fs_name_map>.
          IF sy-subrc IS INITIAL.
            "name mapping found

            IF <fs_name_map>-value IS INITIAL.
              " mapping is missing use prittyfied name as map
              lv_str = zcl_json=>pretty_name( <fs_field>-name ).
*              " mapping is initial so omit this field from output
*              CONTINUE.
            ELSE.
              " mapping found
              lv_str = <fs_name_map>-value.
            ENDIF.
          ELSEIF iv_map_ignore_rest EQ abap_true.
            CONTINUE. "omit field (strict mapping mode)
          ELSE.
            lv_str = zcl_json=>pretty_name( <fs_field>-name ).
          ENDIF.

          me->add(
            EXPORTING
              iv_name    = lv_str
*              ir_element =
              iv_value   = <fv_value>
          ).

        ENDLOOP.

      CATCH cx_root INTO lr_cxroot.
        " Not a Valid Structure Type
        zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Structure' previous = lr_cxroot ).
    ENDTRY.

  ENDMETHOD.


METHOD get.
  FIELD-SYMBOLS: <fs_member> LIKE LINE OF mt_members.

  READ TABLE mt_members ASSIGNING <fs_member> WITH TABLE KEY name = iv_name.
  IF sy-subrc IS INITIAL.
    rr_element = <fs_member>-value.
*  ELSE.
*    rr_element = Zcl_json_null=>gc_instance.
  ENDIF.

ENDMETHOD.


METHOD has.
  rv_exist = abap_false.

  FIELD-SYMBOLS: <fs_member> LIKE LINE OF mt_members.

  READ TABLE mt_members ASSIGNING <fs_member> WITH TABLE KEY name = iv_name.
  IF sy-subrc IS INITIAL.
    rv_exist = abap_true.
  ENDIF.

ENDMETHOD.


METHOD remove.

  FIELD-SYMBOLS: <fs_member> LIKE LINE OF mt_members.

  READ TABLE mt_members ASSIGNING <fs_member> WITH TABLE KEY name = iv_name.
  IF sy-subrc IS INITIAL.
    rr_element = <fs_member>-value.
    DELETE mt_members INDEX sy-tabix.
  ENDIF.

ENDMETHOD.


  METHOD zif_json_element~as_data.
    DATA:
      lr_structdesc TYPE REF TO cl_abap_structdescr,
      lr_cxroot     TYPE REF TO cx_root.

    DATA:
      lt_fields TYPE abap_component_tab,
      lv_str    TYPE string.

    FIELD-SYMBOLS:
      <fv_value>    TYPE any,
      <fs_obj_attr> TYPE zjson_name_element_str,
      <fs_name_map> TYPE zjson_name_value_str,
      <fs_field>    TYPE abap_compdescr.


    TRY .
        lr_structdesc ?= cl_abap_structdescr=>describe_by_data( cv_data ).

        LOOP AT lr_structdesc->components ASSIGNING <fs_field>.
          ASSIGN COMPONENT <fs_field>-name OF STRUCTURE cv_data TO <fv_value>.
          CHECK sy-subrc IS INITIAL.

          "find the name map to json name
          READ TABLE it_names_map WITH KEY name = <fs_field>-name
              ASSIGNING <fs_name_map>.
          IF sy-subrc IS INITIAL.
            "name mapping found

            IF <fs_name_map>-value IS INITIAL.
              lv_str = zcl_json=>pretty_name( <fs_field>-name ).
            ELSE.
              lv_str = <fs_name_map>-value.
            ENDIF.
*            IF <fs_name_map>-value IS INITIAL.
*              " mapping is initial so omit this field from output
*              CONTINUE.
*            ELSE.
            " mapping found
            READ TABLE mt_members WITH KEY name = lv_str
                ASSIGNING <fs_obj_attr>.
            IF sy-subrc IS INITIAL.
              <fs_obj_attr>-value->as_data( EXPORTING is_comp_attrs = <fs_field> CHANGING cv_data =  <fv_value> ).
            ENDIF.
*            ENDIF.
          ELSEIF iv_map_ignore_rest EQ abap_true.
            CONTINUE. "omit field (strict mapping mode)
          ELSE.
            lv_str = zcl_json=>pretty_name( <fs_field>-name ).
            READ TABLE mt_members WITH KEY name = lv_str
                ASSIGNING <fs_obj_attr>.
            IF sy-subrc IS INITIAL.
              <fs_obj_attr>-value->as_data( EXPORTING is_comp_attrs = <fs_field> CHANGING cv_data =  <fv_value> ).
            ENDIF.
          ENDIF.

        ENDLOOP.

      CATCH cx_root INTO lr_cxroot.
        " Not a Valid Structure Type
        zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Structure' previous = lr_cxroot ).
    ENDTRY.
  ENDMETHOD.


  METHOD zif_json_element~as_string.
    rv_string = to_string( ).
  ENDMETHOD.


METHOD Zif_json_element~deep_copy.
  DATA: lr_result TYPE REF TO Zcl_json_object,
        lr_item TYPE REF TO Zif_json_element.

  CREATE OBJECT lr_result.

  lr_item = get_first( ).
  WHILE lr_item IS BOUND.
*    lr_result->add( iv_name = '' ir_element = lr_item->deep_copy( ) ).
    lr_result->add( iv_name = '' ir_element = lr_item ).
    lr_item = get_next( ).
  ENDWHILE.

*  rv_element = me.
  rv_element = lr_result.
ENDMETHOD.


METHOD Zif_json_element~equals.
  DATA: lr_that TYPE REF TO Zcl_json_object.

  FIELD-SYMBOLS: <fr_member> LIKE LINE OF mt_members,
                 <fr_mem_that> LIKE LINE OF mt_members.

  rv_equal = abap_false.

  IF ir_element IS BOUND.
    CHECK ir_element->is_object( ) EQ abap_true. "if other object is an Object?

    lr_that ?= ir_element.

    CHECK lines( mt_members ) EQ lines( lr_that->mt_members ). "if both objects has same properties?

    LOOP AT mt_members ASSIGNING <fr_member>. "deep compare
      READ TABLE lr_that->mt_members ASSIGNING <fr_mem_that> WITH KEY name = <fr_member>-name.
      CHECK sy-subrc IS INITIAL. "no member with the name found in other object, exit

      CHECK <fr_mem_that>-value->equals( ir_element = <fr_member>-value ) EQ abap_true.

    ENDLOOP.

  ELSEIF ir_data IS BOUND.
    RETURN. "not null
  ENDIF.

  rv_equal = abap_true.
ENDMETHOD.


method ZIF_JSON_ELEMENT~GET_ABAP_TYPE.

endmethod.


METHOD Zif_json_element~get_type.
  rv_type = Zif_json_element=>c_type_object.
ENDMETHOD.


METHOD Zif_json_element~is_object.
  iv_return = abap_true.
ENDMETHOD.


METHOD Zif_json_element~to_dref.
  FIELD-SYMBOLS: <fs_member> LIKE LINE OF mt_members,
                 <fs_val> TYPE any,
                 <fv_val> TYPE any,
                 <fs_str> TYPE any.

  DATA: lr_str TYPE REF TO cl_abap_structdescr,
        lt_components TYPE abap_component_tab,
        ls_components TYPE abap_componentdescr,
        lr_dref TYPE REF TO data,
        lv_str TYPE string.


  LOOP AT mt_members ASSIGNING <fs_member>.
    ls_components-name = <fs_member>-name.
    ls_components-type ?= <fs_member>-value->get_abap_type( ).
    APPEND ls_components TO lt_components.
  ENDLOOP.

  lr_str ?= cl_abap_structdescr=>create( lt_components ).

  CREATE DATA rr_dref TYPE HANDLE lr_str.
  ASSIGN rr_dref->* TO <fs_str>.

  LOOP AT mt_members ASSIGNING <fs_member>.
    lv_str = <fs_member>-name.
    TRANSLATE lv_str TO UPPER CASE.
    ASSIGN COMPONENT lv_str OF STRUCTURE <fs_str> TO <fs_val>.
    lr_dref = <fs_member>-value->to_dref( ).
    ASSIGN lr_dref->* TO <fv_val>.
    <fs_val> = <fv_val>.
  ENDLOOP.

ENDMETHOD.


METHOD Zif_json_element~to_string.
* {
*   "string" : "some text",
*   "number" : 123,
*   "boolean" : true,
*   "null" : null,
*   "object" : {
*      "subString" : "value"
*     },
*   "array" : [ 33 , 44 , 488 ]
* }

  rv_string = ''.

  FIELD-SYMBOLS: <fs_member> LIKE LINE OF mt_members.
  DATA: lv_tmp_str TYPE string,
        lv_len TYPE i,
        lv_tabix TYPE i.

  DESCRIBE TABLE mt_members LINES lv_len.

*  IF ir_stream IS BOUND. "write to stream
*    ir_stream->write( `{` ).
*    LOOP AT mt_members ASSIGNING <fs_member>.
*      lv_tabix = sy-tabix.
*
*      rv_string = <fs_member>->to_string( ir_stream = ir_stream ).
*
*      IF lv_len NE lv_tabix.
*        ir_stream->write( `,` ).
*        rv_string = rv_string && `,`.
*      ENDIF.
*    ENDLOOP.
*    ir_stream->write( `}` ).
*    rv_string = rv_string && `}`.
*  ELSE.
*    LOOP AT mt_members ASSIGNING <fs_member>.
*      lv_tabix = sy-tabix.
*
*      lv_tmp_str = <fs_member>->to_string( ir_stream = ir_stream ).
*
*      IF lv_len NE lv_tabix.
*        rv_string = lv_tmp_str && `,`.
*      ELSE.
*        rv_string = lv_tmp_str.
*      ENDIF.
*    ENDLOOP.
*
*    rv_string = `{` && rv_string && `}`.
*  ENDIF.

  IF ir_stream IS BOUND. "write to stream
    ir_stream->write( `{` ).
    LOOP AT mt_members ASSIGNING <fs_member>.
      lv_tabix = sy-tabix.

      CONCATENATE `"` <fs_member>-name `":`  INTO rv_string.
      ir_stream->write( rv_string ).

      <fs_member>-value->to_string( ir_stream = ir_stream ).

      IF lv_len NE lv_tabix.
        ir_stream->write( `,` ).
      ENDIF.
    ENDLOOP.
    ir_stream->write( `}` ).
  ELSE.
    LOOP AT mt_members ASSIGNING <fs_member>.
      lv_tabix = sy-tabix.

      lv_tmp_str = <fs_member>-value->to_string( ir_stream = ir_stream ).

      IF lv_len NE lv_tabix.
        CONCATENATE rv_string `"` <fs_member>-name `":` lv_tmp_str `,` INTO rv_string.
      ELSE.
        CONCATENATE rv_string `"` <fs_member>-name `":` lv_tmp_str INTO rv_string.
      ENDIF.
    ENDLOOP.

    CONCATENATE `{` rv_string `}` INTO rv_string.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_iterator~get_by_index.

  DATA: ls_attr LIKE LINE OF mt_members,
        lr_attr TYPE REF TO Zcl_json_object_attr.

*  IF iv_index LE 0 OR iv_index GT lines( mt_members ).
  IF iv_index LT 0 OR iv_index GE lines( mt_members ).
*    mv_iterator_mark = iv_index.
    " Out of Bounds
    Zcx_json_exception=>raise( iv_msg_number = '002' iv_msgv1 = 'Object' ).
  ENDIF.
  mv_iterator_mark = iv_index + 1.
  READ TABLE mt_members INTO ls_attr INDEX mv_iterator_mark.
  IF sy-subrc IS INITIAL.
    CREATE OBJECT lr_attr
      EXPORTING
        iv_name  = ls_attr-name
        ir_value = ls_attr-value.
    r_result = lr_attr.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_iterator~get_current.

  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO Zcl_json_object_attr.

  READ TABLE mt_members INTO ls_attr INDEX mv_iterator_mark.
  IF sy-subrc IS INITIAL.
    CREATE OBJECT lr_attr
      EXPORTING
        iv_name  = ls_attr-name
        ir_value = ls_attr-value.
    r_result = lr_attr.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_iterator~get_first.
  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO Zcl_json_object_attr.

  mv_iterator_mark = 1.
  IF mv_iterator_mark GT lines( mt_members ).
    SUBTRACT 1 FROM mv_iterator_mark.
    CLEAR: r_result.
*    Zcx_json_exception=>raise( iv_msg = 'Out of Bounds' ).
    RETURN.
  ENDIF.
  READ TABLE mt_members INTO ls_attr INDEX mv_iterator_mark.
  IF sy-subrc IS INITIAL.
    CREATE OBJECT lr_attr
      EXPORTING
        iv_name  = ls_attr-name
        ir_value = ls_attr-value.
    r_result = lr_attr.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_iterator~get_last.
  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO Zcl_json_object_attr.

  mv_iterator_mark = lines( mt_members ).
  READ TABLE mt_members INTO ls_attr INDEX mv_iterator_mark.
  IF sy-subrc IS INITIAL.
    CREATE OBJECT lr_attr
      EXPORTING
        iv_name  = ls_attr-name
        ir_value = ls_attr-value.
    r_result = lr_attr.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_iterator~get_next.

  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO Zcl_json_object_attr.

  ADD 1 TO mv_iterator_mark .

  IF mv_iterator_mark GT lines( mt_members ).
    SUBTRACT 1 FROM mv_iterator_mark.
    CLEAR: r_result.
*    Zcx_json_exception=>raise( iv_msg = 'Out of Bounds' ).
    RETURN.
  ENDIF.

  READ TABLE mt_members INTO ls_attr INDEX mv_iterator_mark.
  IF sy-subrc IS INITIAL.
    CREATE OBJECT lr_attr
      EXPORTING
        iv_name  = ls_attr-name
        ir_value = ls_attr-value.
    r_result = lr_attr.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_iterator~get_previous.

  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO Zcl_json_object_attr.

  SUBTRACT 1 FROM mv_iterator_mark .

  IF mv_iterator_mark GT lines( mt_members ).
    ADD 1 TO mv_iterator_mark.
    CLEAR: r_result.
*    Zcx_json_exception=>raise( iv_msg = 'Out of Bounds' ).
    RETURN.
  ENDIF.

  READ TABLE mt_members INTO ls_attr INDEX mv_iterator_mark.
  IF sy-subrc IS INITIAL.
    CREATE OBJECT lr_attr
      EXPORTING
        iv_name  = ls_attr-name
        ir_value = ls_attr-value.
    r_result = lr_attr.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_iterator~size.
  rv_size = lines( mt_members ).
ENDMETHOD.
ENDCLASS.
