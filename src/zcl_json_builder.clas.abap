class ZCL_JSON_BUILDER definition
  public
  create public .

public section.
  interface ZIF_JSON_ELEMENT load .

  interfaces IF_SERIALIZABLE_OBJECT .

  class-methods CREATE
    importing
      !IV_START_WITH type INT1 default ZIF_JSON_ELEMENT=>C_TYPE_OBJECT
    returning
      value(R_RETURN) type ref to ZCL_JSON_BUILDER .
  methods BUILD
    returning
      value(R_ELEMENT) type ref to ZIF_JSON_ELEMENT .
  methods ADD
    importing
      !IV_NAME type STRING optional
      !IR_ELEMENT type ref to ZIF_JSON_ELEMENT optional
      !IV_VALUE type ANY optional
    returning
      value(R_RETURN) type ref to ZCL_JSON_BUILDER .
  methods START
    importing
      !IV_NAME type STRING optional
      !IV_TYPE type INT1 default ZIF_JSON_ELEMENT=>C_TYPE_OBJECT
    returning
      value(R_RETURN) type ref to ZCL_JSON_BUILDER .
  methods END
    returning
      value(R_RETURN) type ref to ZCL_JSON_BUILDER .
protected section.

  data MT_STACK type ZJSON_ELEMENTS_TAB .
  data MV_STACK_INDEX type I .
  data MR_CURRENT_ELEMENT type ref to ZIF_JSON_ELEMENT .
private section.
ENDCLASS.



CLASS ZCL_JSON_BUILDER IMPLEMENTATION.


METHOD add.

  r_return = me.

  IF mr_current_element->is_object( ) EQ abap_true .
    IF iv_name IS INITIAL.
      " Name can not be initial for an Object
      Zcx_json_exception=>raise( iv_msg_number = '004' ).
    ENDIF.

    DATA: lr_obj TYPE REF TO Zcl_json_object.
    lr_obj ?= mr_current_element.

    lr_obj->add( iv_name = iv_name ir_element = ir_element iv_value = iv_value ).

  ELSEIF mr_current_element->is_array( ) EQ abap_true.

    DATA: lr_arr TYPE REF TO Zcl_json_array.
    lr_arr ?= mr_current_element.

    lr_arr->add( ir_element = ir_element iv_value = iv_value ).

  ELSE.
    " Add operation not supported at current level
    Zcx_json_exception=>raise( iv_msg_number = '005' iv_msgv1 = 'Add' ).

  ENDIF.

ENDMETHOD.


METHOD build.
  IF mv_stack_index IS INITIAL.
    r_element = mr_current_element.
  ELSE.
    READ TABLE mt_stack INDEX 1 INTO r_element.
    CLEAR: mv_stack_index, mt_stack.
  ENDIF.
ENDMETHOD.


METHOD create.

  IF iv_start_with NE Zif_json_element=>c_type_object
    AND iv_start_with NE Zif_json_element=>c_type_array.
    " Not a valid JSON element to start with
    Zcx_json_exception=>raise( iv_msg_number = '003' ).
  ENDIF.

  CREATE OBJECT r_return.

  IF iv_start_with EQ Zif_json_element=>c_type_object.
    CREATE OBJECT r_return->mr_current_element TYPE Zcl_json_object.
  ELSEIF  iv_start_with EQ Zif_json_element=>c_type_array.
    CREATE OBJECT r_return->mr_current_element TYPE Zcl_json_array.
  ENDIF.

ENDMETHOD.


METHOD end.

  r_return = me.

  CHECK mv_stack_index IS NOT INITIAL. "if initial nothing to close

  READ TABLE mt_stack INTO mr_current_element INDEX mv_stack_index.

  DELETE mt_stack INDEX mv_stack_index.

  mv_stack_index = mv_stack_index - 1.

ENDMETHOD.


METHOD start.

  r_return = me.

  IF mr_current_element->is_object( ) EQ abap_true AND iv_name IS INITIAL.
    " Name can not be initial for an Object
    Zcx_json_exception=>raise( iv_msg_number = '004' ).
  ENDIF.

  DATA: lr_element TYPE REF TO Zif_json_element.

  IF iv_type EQ Zif_json_element=>c_type_object.
    CREATE OBJECT lr_element TYPE Zcl_json_object.
  ELSEIF iv_type EQ Zif_json_element=>c_type_array.
    CREATE OBJECT lr_element TYPE Zcl_json_array.
  ELSE.
    " Start Supports only Object/Array types
    Zcx_json_exception=>raise( iv_msg_number = '003' ).
  ENDIF.

  IF mr_current_element->is_object( ) EQ abap_true.
    DATA: lr_obj TYPE REF TO Zcl_json_object.
    lr_obj ?= mr_current_element.
    lr_obj->add( iv_name = iv_name ir_element = lr_element ).
  ELSE.
    DATA: lr_arr TYPE REF TO Zcl_json_array.
    lr_arr ?= mr_current_element.
    lr_arr->add( ir_element = lr_element ).
  ENDIF.

  APPEND mr_current_element TO mt_stack.
  IF sy-subrc IS INITIAL.
    mr_current_element = lr_element.
    mv_stack_index = mv_stack_index + 1.
  ELSE.
    "TODO: throw error
  ENDIF.

ENDMETHOD.
ENDCLASS.
