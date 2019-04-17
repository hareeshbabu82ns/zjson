class ZCL_JSON_OBJECT definition
  public
  inheriting from ZCL_JSON_ELEMENT
  final
  create public .

public section.

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
  type-pools ABAP .
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
  methods ZIF_JSON_ITERATOR~GET_NEXT
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_PREVIOUS
    redefinition .
  methods ZIF_JSON_ITERATOR~SIZE
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_LAST
    redefinition .
protected section.
private section.

  data MT_MEMBERS type ZJSON_NAME_ELEMENT_TAB .
ENDCLASS.



CLASS ZCL_JSON_OBJECT IMPLEMENTATION.


METHOD zif_json_element~deep_copy.
  DATA: lr_result TYPE REF TO zcl_json_object,
        lr_item TYPE REF TO zif_json_element.

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


METHOD zif_json_element~equals.
  DATA: lr_that TYPE REF TO zcl_json_object.

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


METHOD zif_json_element~get_type.
  rv_type = zif_json_element=>c_type_object.
ENDMETHOD.


METHOD zif_json_element~is_object.
  iv_return = abap_true.
ENDMETHOD.


METHOD zif_json_element~to_dref.
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


METHOD zif_json_element~to_string.
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


METHOD zif_json_iterator~get_by_index.

  DATA: ls_attr LIKE LINE OF mt_members,
        lr_attr TYPE REF TO zcl_json_object_attr.

*  IF iv_index LE 0 OR iv_index GT lines( mt_members ).
  IF iv_index LT 0 OR iv_index GE lines( mt_members ).
*    mv_iterator_mark = iv_index.
    " Out of Bounds
    zcx_json_exception=>raise( iv_msg_number = '002' iv_msgv1 = 'Object' ).
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


METHOD zif_json_iterator~get_current.

  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO zcl_json_object_attr.

  READ TABLE mt_members INTO ls_attr INDEX mv_iterator_mark.
  IF sy-subrc IS INITIAL.
    CREATE OBJECT lr_attr
      EXPORTING
        iv_name  = ls_attr-name
        ir_value = ls_attr-value.
    r_result = lr_attr.
  ENDIF.
ENDMETHOD.


METHOD zif_json_iterator~get_first.
  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO zcl_json_object_attr.

  mv_iterator_mark = 1.
  IF mv_iterator_mark GT lines( mt_members ).
    SUBTRACT 1 FROM mv_iterator_mark.
    CLEAR: r_result.
*    zcx_json_exception=>raise( iv_msg = 'Out of Bounds' ).
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


METHOD zif_json_iterator~get_last.
  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO zcl_json_object_attr.

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


METHOD zif_json_iterator~get_next.

  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO zcl_json_object_attr.

  ADD 1 TO mv_iterator_mark .

  IF mv_iterator_mark GT lines( mt_members ).
    SUBTRACT 1 FROM mv_iterator_mark.
    CLEAR: r_result.
*    zcx_json_exception=>raise( iv_msg = 'Out of Bounds' ).
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


METHOD zif_json_iterator~get_previous.

  DATA: ls_attr LIKE LINE OF mt_members,
      lr_attr TYPE REF TO zcl_json_object_attr.

  SUBTRACT 1 FROM mv_iterator_mark .

  IF mv_iterator_mark GT lines( mt_members ).
    ADD 1 TO mv_iterator_mark.
    CLEAR: r_result.
*    zcx_json_exception=>raise( iv_msg = 'Out of Bounds' ).
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


METHOD zif_json_iterator~size.
  rv_size = lines( mt_members ).
ENDMETHOD.


METHOD add.
  DATA: ls_member TYPE zjson_name_element_str,
        lr_attr TYPE REF TO zcl_json_object_attr.

  ls_member-name = iv_name.

  IF ir_element IS NOT BOUND.
    IF iv_value IS SUPPLIED.
      CREATE OBJECT ls_member-value TYPE zcl_json_primitive
        EXPORTING
          ir_value = iv_value.
    ELSE.
      ls_member-value = zcl_json_null=>get_instance( ).
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


METHOD get.
  FIELD-SYMBOLS: <fs_member> LIKE LINE OF mt_members.

  READ TABLE mt_members ASSIGNING <fs_member> WITH TABLE KEY name = iv_name.
  IF sy-subrc IS INITIAL.
    rr_element = <fs_member>-value.
*  ELSE.
*    rr_element = zcl_json_null=>gc_instance.
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
ENDCLASS.
