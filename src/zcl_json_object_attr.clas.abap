class ZCL_JSON_OBJECT_ATTR definition
  public
  inheriting from ZCL_JSON_ELEMENT
  final
  create public .

public section.

  data NAME type STRING read-only .
  data VALUE type ref to ZIF_JSON_ELEMENT read-only .

  methods CONSTRUCTOR
    importing
      !IV_NAME type STRING
      !IR_VALUE type ref to ZIF_JSON_ELEMENT .
  methods SET_NAME
    importing
      !IV_NAME type STRING .
  methods SET_VALUE
    importing
      !IR_VALUE type ref to ZIF_JSON_ELEMENT .

  methods ZIF_JSON_ELEMENT~GET_TYPE
    redefinition .
  methods ZIF_JSON_ELEMENT~IS_OBJECT_ATTRIBUTE
    redefinition .
  methods ZIF_JSON_ELEMENT~TO_STRING
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_JSON_OBJECT_ATTR IMPLEMENTATION.


METHOD constructor.
  super->constructor( ).
  name = iv_name.
  value = ir_value.
ENDMETHOD.


METHOD set_name.
  name = iv_name.
ENDMETHOD.


METHOD set_value.
  value = ir_value.
ENDMETHOD.


METHOD Zif_json_element~get_type.
  rv_type = Zif_json_element=>c_type_object_attribute.
ENDMETHOD.


METHOD Zif_json_element~is_object_attribute.
  iv_return = abap_true.
ENDMETHOD.


METHOD Zif_json_element~to_string.
*   "string" : "some text",
*   "number" : 123,
*   "boolean" : true,
*   "null" : null,
*   "object" : {
*      "subString" : "value"
*     },
*   "array" : [ 33 , 44 , 488 ]

  rv_string = ''.

  DATA: lv_tmp_str TYPE string,
        lv_len TYPE i,
        lv_tabix TYPE i.

  IF ir_stream IS BOUND. "write to stream
    rv_string = `"` && name && `":`.
    ir_stream->write( rv_string ).
    lv_tmp_str = value->to_string( ir_stream = ir_stream ).
    rv_string = rv_string && lv_tmp_str.
  ELSE.
    lv_tmp_str = value->to_string( ir_stream = ir_stream ).
    rv_string = `"` && name && `":` && lv_tmp_str.
  ENDIF.
ENDMETHOD.
ENDCLASS.
