
CLASS ztest_json_object DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ztest_Json_Object
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_JSON_OBJECT
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

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS:
      create_from_data FOR TESTING,
      convert_to_data FOR TESTING.
ENDCLASS.       "ztest_Json_Object


CLASS ztest_json_object IMPLEMENTATION.

  METHOD class_setup.



  ENDMETHOD.


  METHOD class_teardown.



  ENDMETHOD.


  METHOD setup.


  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.

  METHOD convert_to_data.

    TYPES:
      BEGIN OF lty_sample_table,
        field1  TYPE string,
        field_2 TYPE i,
        field3  TYPE dats,
        field4  TYPE string,
      END OF lty_sample_table.

    DATA:
      ls_sample     TYPE lty_sample_table,
      ls_sample_cpy TYPE lty_sample_table,
      lt_name_map   TYPE zjson_name_value_tab,
      ls_name_map   TYPE zjson_name_value_str.

    ls_sample-field1 = 'test1'.
    ls_sample-field_2 = 1.
    ls_sample-field3 = '20140201'.
    ls_sample-field4 = 'ignored'.

    DATA: lr_obj TYPE REF TO zcl_json_object.

    "default conversion
    CREATE OBJECT lr_obj
      EXPORTING
        is_data = ls_sample.

    CLEAR ls_sample_cpy.
    lr_obj->as_data( CHANGING cv_data = ls_sample_cpy ).

    cl_abap_unit_assert=>assert_equals( act = ls_sample_cpy exp = ls_sample ).

    "conversion with mapping (also filters)

    ls_name_map-name = 'FIELD1'.
    ls_name_map-value = 'toField1'.
    APPEND ls_name_map TO lt_name_map.

    ls_name_map-name = 'FIELD_2'.
    ls_name_map-value = ''. "use default mapping
    APPEND ls_name_map TO lt_name_map.

    ls_name_map-name = 'FIELD3'.
    ls_name_map-value = 'field_3'.
    APPEND ls_name_map TO lt_name_map.

    CREATE OBJECT lr_obj
      EXPORTING
        is_data      = ls_sample
        it_names_map = lt_name_map.

    CLEAR ls_sample_cpy.
    lr_obj->as_data( EXPORTING it_names_map = lt_name_map CHANGING cv_data = ls_sample_cpy  ).

    cl_abap_unit_assert=>assert_equals( act = ls_sample_cpy-field1 exp = ls_sample-field1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_sample_cpy-field3 exp = ls_sample-field3 ).
    cl_abap_unit_assert=>assert_true( act = lr_obj->has( 'field2' ) ).
    cl_abap_unit_assert=>assert_equals( act = ls_sample_cpy-field4 exp = ls_sample-field4 ).

    CREATE OBJECT lr_obj
      EXPORTING
        is_data            = ls_sample
        it_names_map       = lt_name_map
        iv_map_ignore_rest = abap_true.

    CLEAR ls_sample_cpy.
    lr_obj->as_data( EXPORTING it_names_map = lt_name_map iv_map_ignore_rest = abap_true
            CHANGING cv_data = ls_sample_cpy  ).
    cl_abap_unit_assert=>assert_equals( act = ls_sample_cpy-field1 exp = ls_sample-field1 ).
    cl_abap_unit_assert=>assert_equals( act = ls_sample_cpy-field3 exp = ls_sample-field3 ).
    cl_abap_unit_assert=>assert_true( act = boolc( ls_sample_cpy-field_2 EQ ls_sample-field_2 ) ).
    cl_abap_unit_assert=>assert_true( act = boolc( ls_sample_cpy-field4 NE ls_sample-field4 ) ).

  ENDMETHOD.

  METHOD create_from_data.

    TYPES:
      BEGIN OF lty_sample_table,
        field1  TYPE string,
        field_2 TYPE i,
        field3  TYPE dats,
        field4  TYPE string,
      END OF lty_sample_table.

    DATA:
      ls_sample   TYPE lty_sample_table,
      lt_name_map TYPE zjson_name_value_tab,
      ls_name_map TYPE zjson_name_value_str.

    ls_sample-field1 = 'test1'.
    ls_sample-field_2 = 1.
    ls_sample-field3 = '20140201'.
    ls_sample-field4 = 'ignored'.

    DATA: lr_obj TYPE REF TO zcl_json_object.

    "default conversion
    CREATE OBJECT lr_obj
      EXPORTING
        is_data = ls_sample.

    cl_abap_unit_assert=>assert_equals( act = lr_obj->get( 'field1' )->as_string( )
         exp = 'test1' msg = 'Object values Mismatch').

    "conversion with mapping (also filters)

    ls_name_map-name = 'FIELD1'.
    ls_name_map-value = 'toField1'.
    APPEND ls_name_map TO lt_name_map.

    ls_name_map-name = 'FIELD_2'.
    ls_name_map-value = ''. "use default mapping
    APPEND ls_name_map TO lt_name_map.

    ls_name_map-name = 'FIELD3'.
    ls_name_map-value = 'field_3'.
    APPEND ls_name_map TO lt_name_map.

    CREATE OBJECT lr_obj
      EXPORTING
        is_data      = ls_sample
        it_names_map = lt_name_map.

    cl_abap_unit_assert=>assert_equals( act = lr_obj->has( 'field1' ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lr_obj->has( 'field_2' ) exp = abap_false ).
    cl_abap_unit_assert=>assert_true( act = lr_obj->has( 'field2' ) ).
    cl_abap_unit_assert=>assert_equals( act = lr_obj->has( 'field_3' ) exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = lr_obj->has( 'field4' ) exp = abap_true ).

    cl_abap_unit_assert=>assert_equals( act = lr_obj->get( 'toField1' )->as_string( )
          exp = 'test1' ).

    CREATE OBJECT lr_obj
      EXPORTING
        is_data            = ls_sample
        it_names_map       = lt_name_map
        iv_map_ignore_rest = abap_true.
    cl_abap_unit_assert=>assert_equals( act = lr_obj->has( 'field1' ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lr_obj->has( 'field_2' ) exp = abap_false ).
    cl_abap_unit_assert=>assert_equals( act = lr_obj->has( 'field_3' ) exp = abap_true ).
    cl_abap_unit_assert=>assert_equals( act = lr_obj->has( 'field4' ) exp = abap_false ).

    cl_abap_unit_assert=>assert_equals( act = lr_obj->get( 'toField1' )->as_string( )
          exp = 'test1' ).

  ENDMETHOD.



ENDCLASS.
