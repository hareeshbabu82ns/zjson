
CLASS ztest_json_array DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ztest_Json_Array
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_JSON_ARRAY
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_json_array.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS:
      create_from_data FOR TESTING,
      convert_to_data FOR TESTING.
ENDCLASS.       "ztest_Json_Array


CLASS ztest_json_array IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.

    DATA it_names_map TYPE zjson_name_value_tab.


  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.

  METHOD convert_to_data.

    TYPES:
      BEGIN OF lty_sample_table,
        field1  TYPE string,
        field_2 TYPE i,
        field3  TYPE dats,
      END OF lty_sample_table.

    DATA: lt_sample     TYPE STANDARD TABLE OF lty_sample_table,
          lt_sample_cpy TYPE STANDARD TABLE OF lty_sample_table,
          ls_sample     TYPE lty_sample_table.

    ls_sample-field1 = 'test1'.
    ls_sample-field_2 = 1.
    ls_sample-field3 = '20140201'.
    APPEND ls_sample TO lt_sample.

    DATA: lr_arr TYPE REF TO zcl_json_array.

    CREATE OBJECT lr_arr
      EXPORTING
        it_data = lt_sample.

    CLEAR lt_sample_cpy.
    lr_arr->as_data( CHANGING cv_data = lt_sample_cpy ).

    cl_abap_unit_assert=>assert_equals( act = lt_sample_cpy exp = lt_sample msg = 'Array Mismatch').


    DATA: lt_str     TYPE STANDARD TABLE OF string,
          lt_str_cpy TYPE STANDARD TABLE OF string.

    APPEND 'test' TO lt_str.
    APPEND 'test2' TO lt_str.

    CREATE OBJECT lr_arr
      EXPORTING
        it_data = lt_str.

    CLEAR lt_sample_cpy.
    lr_arr->as_data( CHANGING cv_data = lt_str_cpy ).

    cl_abap_unit_assert=>assert_equals( act = lt_str_cpy exp = lt_str msg = 'Array Mismatch').

  ENDMETHOD.

  METHOD create_from_data.

    TYPES:
      BEGIN OF lty_sample_table,
        field1  TYPE string,
        field_2 TYPE i,
        field3  TYPE dats,
      END OF lty_sample_table.

    DATA: lt_sample TYPE STANDARD TABLE OF lty_sample_table,
          ls_sample TYPE lty_sample_table.

    ls_sample-field1 = 'test1'.
    ls_sample-field_2 = 1.
    ls_sample-field3 = '20140201'.
    APPEND ls_sample TO lt_sample.

    DATA: lr_arr TYPE REF TO zcl_json_array.

    CREATE OBJECT lr_arr
      EXPORTING
        it_data = lt_sample.

    cl_abap_unit_assert=>assert_equals( act = lr_arr->length( ) exp = 1 msg = 'Array Length Mismatch').


    DATA: lt_str TYPE STANDARD TABLE OF string.

    APPEND 'test' TO lt_str.
    APPEND 'test2' TO lt_str.

    CREATE OBJECT lr_arr
      EXPORTING
        it_data = lt_str.

    cl_abap_unit_assert=>assert_equals( act = lr_arr->length( ) exp = 2 msg = 'Array Length Mismatch').


  ENDMETHOD.




ENDCLASS.
