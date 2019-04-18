class ZCL_JSON_ELEMENT definition
  public
  abstract
  create public .

public section.

  interfaces ZIF_JSON_ELEMENT .
  interfaces ZIF_JSON_ITERATOR .

  aliases C_TYPE_ARRAY
    for ZIF_JSON_ELEMENT~C_TYPE_ARRAY .
  aliases C_TYPE_NULL
    for ZIF_JSON_ELEMENT~C_TYPE_NULL .
  aliases C_TYPE_OBJECT
    for ZIF_JSON_ELEMENT~C_TYPE_OBJECT .
  aliases C_TYPE_OBJECT_ATTRIBUTE
    for ZIF_JSON_ELEMENT~C_TYPE_OBJECT_ATTRIBUTE .
  aliases C_TYPE_PRIMITIVE
    for ZIF_JSON_ELEMENT~C_TYPE_PRIMITIVE .
  aliases C_TYPE_PRIMITIVE_BOOLEAN
    for ZIF_JSON_ELEMENT~C_TYPE_PRIMITIVE_BOOLEAN .
  aliases C_TYPE_PRIMITIVE_NUMBER
    for ZIF_JSON_ELEMENT~C_TYPE_PRIMITIVE_NUMBER .
  aliases C_TYPE_PRIMITIVE_STRING
    for ZIF_JSON_ELEMENT~C_TYPE_PRIMITIVE_STRING .
  aliases C_VALUE_BOOL_FALSE
    for ZIF_JSON_ELEMENT~C_VALUE_BOOL_FALSE .
  aliases C_VALUE_BOOL_TRUE
    for ZIF_JSON_ELEMENT~C_VALUE_BOOL_TRUE .
  aliases C_VALUE_NULL
    for ZIF_JSON_ELEMENT~C_VALUE_NULL .
  aliases AS_DATA
    for ZIF_JSON_ELEMENT~AS_DATA .
  aliases AS_STRING
    for ZIF_JSON_ELEMENT~AS_STRING .
  aliases DEEP_COPY
    for ZIF_JSON_ELEMENT~DEEP_COPY .
  aliases EQUALS
    for ZIF_JSON_ELEMENT~EQUALS .
  aliases GET_ABAP_TYPE
    for ZIF_JSON_ELEMENT~GET_ABAP_TYPE .
  aliases GET_BY_INDEX
    for ZIF_JSON_ITERATOR~GET_BY_INDEX .
  aliases GET_CURRENT
    for ZIF_JSON_ITERATOR~GET_CURRENT .
  aliases GET_CURRENT_INDEX
    for ZIF_JSON_ITERATOR~GET_CURRENT_INDEX .
  aliases GET_FIRST
    for ZIF_JSON_ITERATOR~GET_FIRST .
  aliases GET_LAST
    for ZIF_JSON_ITERATOR~GET_LAST .
  aliases GET_NEXT
    for ZIF_JSON_ITERATOR~GET_NEXT .
  aliases GET_PREVIOUS
    for ZIF_JSON_ITERATOR~GET_PREVIOUS .
  aliases GET_TYPE
    for ZIF_JSON_ELEMENT~GET_TYPE .
  aliases IS_ARRAY
    for ZIF_JSON_ELEMENT~IS_ARRAY .
  aliases IS_BOOLEAN
    for ZIF_JSON_ELEMENT~IS_BOOLEAN .
  aliases IS_NULL
    for ZIF_JSON_ELEMENT~IS_NULL .
  aliases IS_NUMBER
    for ZIF_JSON_ELEMENT~IS_NUMBER .
  aliases IS_OBJECT
    for ZIF_JSON_ELEMENT~IS_OBJECT .
  aliases IS_OBJECT_ATTRIBUTE
    for ZIF_JSON_ELEMENT~IS_OBJECT_ATTRIBUTE .
  aliases IS_STRING
    for ZIF_JSON_ELEMENT~IS_STRING .
  aliases SIZE
    for ZIF_JSON_ITERATOR~SIZE .
  aliases TO_DREF
    for ZIF_JSON_ELEMENT~TO_DREF .
  aliases TO_STRING
    for ZIF_JSON_ELEMENT~TO_STRING .
protected section.

  data MV_ITERATOR_MARK type INT2 value 0. "#EC NOTEXT
private section.
ENDCLASS.



CLASS ZCL_JSON_ELEMENT IMPLEMENTATION.


METHOD Zif_json_element~as_data.
  CLEAR cv_data.
ENDMETHOD.


METHOD Zif_json_element~as_string.
  CLEAR rv_string.
ENDMETHOD.


method ZIF_JSON_ELEMENT~DEEP_COPY.
endmethod.


method ZIF_JSON_ELEMENT~EQUALS.
endmethod.


method ZIF_JSON_ELEMENT~GET_ABAP_TYPE.
endmethod.


method ZIF_JSON_ELEMENT~GET_TYPE.
endmethod.


method ZIF_JSON_ELEMENT~IS_ARRAY.
  iv_return = abap_false.
endmethod.


method ZIF_JSON_ELEMENT~IS_BOOLEAN.
  iv_return = abap_false.
endmethod.


method ZIF_JSON_ELEMENT~IS_NULL.
  iv_return = abap_false.
endmethod.


METHOD Zif_json_element~is_number.
  iv_return = abap_false.
ENDMETHOD.


method ZIF_JSON_ELEMENT~IS_OBJECT.
  iv_return = abap_false.
endmethod.


METHOD Zif_json_element~is_object_attribute.
  iv_return = abap_false.
ENDMETHOD.


method ZIF_JSON_ELEMENT~IS_STRING.
  iv_return = abap_false.
endmethod.


method ZIF_JSON_ELEMENT~TO_DREF.
endmethod.


method ZIF_JSON_ELEMENT~TO_STRING.
endmethod.


method ZIF_JSON_ITERATOR~GET_BY_INDEX.
endmethod.


method ZIF_JSON_ITERATOR~GET_CURRENT.
endmethod.


METHOD Zif_json_iterator~get_current_index.
  rv_result = mv_iterator_mark.
ENDMETHOD.


METHOD Zif_json_iterator~get_first.
ENDMETHOD.


method ZIF_JSON_ITERATOR~GET_LAST.
endmethod.


method ZIF_JSON_ITERATOR~GET_NEXT.
endmethod.


method ZIF_JSON_ITERATOR~GET_PREVIOUS.
endmethod.


METHOD Zif_json_iterator~size.
  rv_size = -1.
ENDMETHOD.
ENDCLASS.
