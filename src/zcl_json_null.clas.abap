class ZCL_JSON_NULL definition
  public
  inheriting from ZCL_JSON_ELEMENT
  final
  create protected .

public section.

  class-data GC_INSTANCE type ref to ZCL_JSON_NULL read-only .

  methods CONSTRUCTOR .
  class-methods GET_INSTANCE
    returning
      value(RV_NULL) type ref to ZCL_JSON_NULL .
  class-methods CLASS_CONSTRUCTOR .

  methods ZIF_JSON_ELEMENT~DEEP_COPY
    redefinition .
  methods ZIF_JSON_ELEMENT~EQUALS
    redefinition .
  methods ZIF_JSON_ELEMENT~GET_ABAP_TYPE
    redefinition .
  methods ZIF_JSON_ELEMENT~GET_TYPE
    redefinition .
  methods ZIF_JSON_ELEMENT~TO_DREF
    redefinition .
  methods ZIF_JSON_ELEMENT~TO_STRING
    redefinition .
  methods ZIF_JSON_ELEMENT~IS_NULL
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_JSON_NULL IMPLEMENTATION.


METHOD zif_json_element~deep_copy.
  rv_element = gc_instance.
ENDMETHOD.


METHOD zif_json_element~equals.
  rv_equal = abap_false.
  IF ir_element IS BOUND.
    CHECK me EQ ir_element AND
      get_type( ) EQ ir_element->get_type( ).
  ELSEIF ir_data IS BOUND.
    RETURN. "not null
  ENDIF.
  rv_equal = abap_true.
ENDMETHOD.


method ZIF_JSON_ELEMENT~GET_ABAP_TYPE.
*CALL METHOD SUPER->ZIF_JSON_ELEMENT~GET_ABAP_TYPE
*  RECEIVING
*    RR_TYPE =
*    .
endmethod.


METHOD zif_json_element~get_type.
  rv_type = zif_json_element=>c_type_null.
ENDMETHOD.


METHOD zif_json_element~is_null.
  iv_return = abap_true.
ENDMETHOD.


method ZIF_JSON_ELEMENT~TO_DREF.
*CALL METHOD SUPER->ZIF_JSON_ELEMENT~TO_DREF
*  RECEIVING
*    RR_DREF =
*    .
endmethod.


METHOD zif_json_element~to_string.
  IF ir_stream IS BOUND.
    ir_stream->write( zif_json_element=>c_value_null ).
  ELSE.
    rv_string = zif_json_element=>c_value_null.
  ENDIF.
ENDMETHOD.


METHOD class_constructor.
  CREATE OBJECT gc_instance.
ENDMETHOD.


METHOD constructor.
  super->constructor( ).
ENDMETHOD.


METHOD GET_INSTANCE.
  IF gc_instance IS NOT BOUND.
    CREATE OBJECT gc_instance.
  ENDIF.
  rv_null = gc_instance.
ENDMETHOD.
ENDCLASS.
