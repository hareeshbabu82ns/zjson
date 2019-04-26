CLASS zcl_json_builder DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACE zif_json_element LOAD .

    INTERFACES if_serializable_object .

    CLASS-METHODS create
      IMPORTING
        !iv_start_with  TYPE int1 DEFAULT zif_json_element=>c_type_object
      RETURNING
        VALUE(r_return) TYPE REF TO zcl_json_builder .
    METHODS build
      RETURNING
        VALUE(r_element) TYPE REF TO zif_json_element .
    METHODS add
      IMPORTING
        !iv_name        TYPE string OPTIONAL
        !ir_element     TYPE REF TO zif_json_element OPTIONAL
        !iv_value       TYPE any OPTIONAL
      RETURNING
        VALUE(r_return) TYPE REF TO zcl_json_builder .
    METHODS start
      IMPORTING
        !iv_name        TYPE string OPTIONAL
        !iv_type        TYPE int1 DEFAULT zif_json_element=>c_type_object
      RETURNING
        VALUE(r_return) TYPE REF TO zcl_json_builder .
    METHODS end
      RETURNING
        VALUE(r_return) TYPE REF TO zcl_json_builder .
  PROTECTED SECTION.

    DATA mt_stack TYPE zjson_elements_tab .
    DATA mv_stack_index TYPE i .
    DATA mr_current_element TYPE REF TO zif_json_element .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_json_builder IMPLEMENTATION.


  METHOD add.

    r_return = me.

    IF mr_current_element->is_object( ) EQ abap_true .
      IF iv_name IS INITIAL.
        " Name can not be initial for an Object
        zcx_json_exception=>raise( iv_msg_number = '004' ).
      ENDIF.

      DATA: lr_obj TYPE REF TO zcl_json_object.
      lr_obj ?= mr_current_element.

      lr_obj->add( iv_name = iv_name ir_element = ir_element iv_value = iv_value ).

    ELSEIF mr_current_element->is_array( ) EQ abap_true.

      DATA: lr_arr TYPE REF TO zcl_json_array.
      lr_arr ?= mr_current_element.

      lr_arr->add( ir_element = ir_element iv_value = iv_value ).

    ELSE.
      " Add operation not supported at current level
      zcx_json_exception=>raise( iv_msg_number = '005' iv_msgv1 = 'Add' ).

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

    IF iv_start_with NE zif_json_element=>c_type_object
      AND iv_start_with NE zif_json_element=>c_type_array.
      " Not a valid JSON element to start with
      zcx_json_exception=>raise( iv_msg_number = '003' ).
    ENDIF.

    CREATE OBJECT r_return.

    IF iv_start_with EQ zif_json_element=>c_type_object.
      CREATE OBJECT r_return->mr_current_element TYPE zcl_json_object.
    ELSEIF  iv_start_with EQ zif_json_element=>c_type_array.
      CREATE OBJECT r_return->mr_current_element TYPE zcl_json_array.
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
      zcx_json_exception=>raise( iv_msg_number = '004' ).
    ENDIF.

    DATA: lr_element TYPE REF TO zif_json_element.

    IF iv_type EQ zif_json_element=>c_type_object.
      CREATE OBJECT lr_element TYPE zcl_json_object.
    ELSEIF iv_type EQ zif_json_element=>c_type_array.
      CREATE OBJECT lr_element TYPE zcl_json_array.
    ELSE.
      " Start Supports only Object/Array types
      zcx_json_exception=>raise( iv_msg_number = '003' ).
    ENDIF.

    IF mr_current_element->is_object( ) EQ abap_true.
      DATA: lr_obj TYPE REF TO zcl_json_object.
      lr_obj ?= mr_current_element.
      lr_obj->add( iv_name = iv_name ir_element = lr_element ).
    ELSE.
      DATA: lr_arr TYPE REF TO zcl_json_array.
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
