interface ZIF_JSON_ELEMENT
  public .


  interfaces IF_SERIALIZABLE_OBJECT .

  constants C_TYPE_ARRAY type INT1 value 1. "#EC NOTEXT
  constants C_TYPE_OBJECT type INT1 value 2. "#EC NOTEXT
  constants C_TYPE_PRIMITIVE type INT1 value 3. "#EC NOTEXT
  constants C_TYPE_NULL type INT1 value 4. "#EC NOTEXT
  constants C_TYPE_PRIMITIVE_NUMBER type INT1 value 5. "#EC NOTEXT
  constants C_TYPE_PRIMITIVE_STRING type INT1 value 6. "#EC NOTEXT
  constants C_TYPE_PRIMITIVE_BOOLEAN type INT1 value 7. "#EC NOTEXT
  constants C_TYPE_OBJECT_ATTRIBUTE type INT1 value 8. "#EC NOTEXT
  constants C_VALUE_NULL type STRING value 'null'. "#EC NOTEXT
  constants C_VALUE_BOOL_TRUE type STRING value 'true'. "#EC NOTEXT
  constants C_VALUE_BOOL_FALSE type STRING value 'false'. "#EC NOTEXT

  methods DEEP_COPY
    returning
      value(RV_ELEMENT) type ref to ZIF_JSON_ELEMENT .
  methods TO_STRING
    importing
      !IR_STREAM type ref to CL_ABAP_STRING_C_WRITER optional
    returning
      value(RV_STRING) type STRING .
  methods GET_TYPE
    returning
      value(RV_TYPE) type INT1 .
  methods TO_DREF
    returning
      value(RR_DREF) type ref to DATA .
  methods GET_ABAP_TYPE
    returning
      value(RR_TYPE) type ref to CL_ABAP_TYPEDESCR .
  type-pools ABAP .
  methods EQUALS
    importing
      !IR_DATA type ref to DATA optional
      !IR_ELEMENT type ref to ZIF_JSON_ELEMENT optional
    returning
      value(RV_EQUAL) type ABAP_BOOL .
  methods IS_NUMBER
    returning
      value(IV_RETURN) type ABAP_BOOL .
  methods IS_STRING
    returning
      value(IV_RETURN) type ABAP_BOOL .
  methods IS_BOOLEAN
    returning
      value(IV_RETURN) type ABAP_BOOL .
  methods IS_NULL
    returning
      value(IV_RETURN) type ABAP_BOOL .
  methods IS_OBJECT
    returning
      value(IV_RETURN) type ABAP_BOOL .
  methods IS_ARRAY
    returning
      value(IV_RETURN) type ABAP_BOOL .
  methods AS_STRING
    returning
      value(RV_STRING) type STRING .
  methods AS_DATA
    importing
      !IS_COMP_ATTRS type ABAP_COMPDESCR optional
    changing
      value(CV_DATA) type ANY .
  methods IS_OBJECT_ATTRIBUTE
    returning
      value(IV_RETURN) type ABAP_BOOL .
endinterface.
