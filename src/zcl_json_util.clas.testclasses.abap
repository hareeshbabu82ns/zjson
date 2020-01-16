
CLASS ztest_json_util DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ztest_Json_Util
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_JSON_UTIL
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
* ================

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: get_success FOR TESTING,
      get_fail FOR TESTING.
    METHODS: prettefy_json_to_abap FOR TESTING,
      prettefy_json_to_abap_fail FOR TESTING.
ENDCLASS.       "ztest_Json_Util


CLASS ztest_json_util IMPLEMENTATION.

  METHOD class_setup.
* ===================


  ENDMETHOD.       "class_Setup


  METHOD class_teardown.
* ======================


  ENDMETHOD.       "class_Teardown


  METHOD setup.
* =============


  ENDMETHOD.       "setup


  METHOD teardown.
* ================


  ENDMETHOD.       "teardown

  METHOD prettefy_json_to_abap.
    DATA: lv_json_name     TYPE string,
          lv_abap_name     TYPE string,
          lv_abap_name_act TYPE string.

    lv_abap_name = 'ABAP_NAME'.
    lv_json_name = zcl_json_util=>prettify_abap_name( lv_abap_name ).
    lv_abap_name_act = zcl_json_util=>prettify_to_abap_name( lv_json_name ).

    cl_abap_unit_assert=>assert_equals( act = lv_abap_name_act exp = lv_abap_name
      msg = |ABAP name conversion failed for { lv_abap_name }| ).

    lv_abap_name = 'ABAP_NAME93'.
    lv_json_name = zcl_json_util=>prettify_abap_name( lv_abap_name ).
    lv_abap_name_act = zcl_json_util=>prettify_to_abap_name( lv_json_name ).

    cl_abap_unit_assert=>assert_equals( act = lv_abap_name_act exp = lv_abap_name
      msg = |ABAP name conversion failed for { lv_abap_name }| ).

  ENDMETHOD.

  METHOD prettefy_json_to_abap_fail.
    DATA: lv_json_name     TYPE string,
          lv_abap_name     TYPE string,
          lv_abap_name_act TYPE string.

    lv_abap_name = '/ABAP_NAME'.
    lv_json_name = zcl_json_util=>prettify_abap_name( lv_abap_name ).
    lv_abap_name_act = zcl_json_util=>prettify_to_abap_name( lv_json_name ).

    cl_abap_unit_assert=>assert_false(
      act = boolc( lv_abap_name_act EQ lv_abap_name )
      msg = |ABAP name conversion should fail for { lv_abap_name }| ).

    lv_abap_name = '_ABAP_NAME'.
    lv_json_name = zcl_json_util=>prettify_abap_name( lv_abap_name ).
    lv_abap_name_act = zcl_json_util=>prettify_to_abap_name( lv_json_name ).

    cl_abap_unit_assert=>assert_false(
      act = boolc( lv_abap_name_act EQ lv_abap_name )
      msg = |ABAP name conversion should fail for { lv_abap_name }| ).

    lv_abap_name = 'ABAP_NAME_9'.
    lv_json_name = zcl_json_util=>prettify_abap_name( lv_abap_name ).
    lv_abap_name_act = zcl_json_util=>prettify_to_abap_name( lv_json_name ).

    cl_abap_unit_assert=>assert_false(
      act = boolc( lv_abap_name_act EQ lv_abap_name )
      msg = |ABAP name conversion should fail for { lv_abap_name }| ).

    lv_abap_name = 'ABAP~NAME'.
    lv_json_name = zcl_json_util=>prettify_abap_name( lv_abap_name ).
    lv_abap_name_act = zcl_json_util=>prettify_to_abap_name( lv_json_name ).

    cl_abap_unit_assert=>assert_false(
      act = boolc( lv_abap_name_act EQ lv_abap_name )
      msg = |ABAP name conversion should fail for { lv_abap_name }| ).
  ENDMETHOD.

  METHOD get_success.
* ===========
    DATA lv_exp_result TYPE string.
    DATA lr_json_array TYPE REF TO zif_json_element.
    DATA lr_json_object TYPE REF TO zif_json_element.
    DATA lr_json_result TYPE REF TO zif_json_element.


    zcl_json_parser=>from_json(
            EXPORTING json = `{` &&
                `"attr1":"value","attr2":"value2",` &&
                `"arr1":["arrValue1","arrValue2",` &&
                `{"deepArrAttr1":"deepArrVal1"},` &&
                `{"deepArrAttr1":"deepArrVal2"}],` &&
                `"deepObj":{ "attr1":"deepValue1" }` &&
              `}`
            CHANGING json_element = lr_json_object ).

    " Get Object Attribute (array)
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'arr1' ).

    cl_abap_unit_assert=>assert_true(
      act   = lr_json_result->is_array( ) ).

    " Get unknown Object Attribute - No Default return null
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'attr3' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lr_json_result
      exp   = zcl_json_null=>gc_instance ).

    " Get unknown deep Object Attribute - No Default return null
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'deepObj.attrUnknown' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lr_json_result
      exp   = zcl_json_null=>gc_instance ).

    " Get unknown Object Attribute - Default Value
    lv_exp_result = 'test'.
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'attrUnknown'
        iv_default = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lr_json_result->as_string( )
      exp   = lv_exp_result ).

    " Get Object Attribute
    lv_exp_result = 'value2'.
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'attr2'
        iv_default = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lr_json_result->as_string( )
      exp   = lv_exp_result ).

    " Get deep Object Attribute
    lv_exp_result = 'deepValue1'.
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'deepObj.attr1' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lr_json_result->as_string( )
      exp   = lv_exp_result ).

    " Get Array Attribute with Index
    lv_exp_result = 'arrValue2'.
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'arr1[1]'
        iv_default = 'test' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lr_json_result->as_string( )
      exp   = lv_exp_result ).

    " Get unknown deep Array Attribute with Index - default
    lv_exp_result = 'default'.
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'arr1[1].deepArrAttr1'
        iv_default = lv_exp_result ).

    cl_abap_unit_assert=>assert_equals(
      act   = lr_json_result->as_string( )
      exp   = lv_exp_result ).

    " Get deep Array Attribute with Index
    lv_exp_result = 'deepArrVal1'.
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'arr1[2].deepArrAttr1' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lr_json_result->as_string( )
      exp   = lv_exp_result ).

    " Get deep Array Attribute with SubObject Attribute
    lv_exp_result = 'deepArrVal2'.
    lr_json_result = zcl_json_util=>get(
        ir_json_object = lr_json_object
        iv_path = 'arr1[{"deepArrAttr1":"deepArrVal2"}].deepArrAttr1' ).

    cl_abap_unit_assert=>assert_equals(
      act   = lr_json_result->as_string( )
      exp   = lv_exp_result ).

  ENDMETHOD.       "get_success

  METHOD get_fail.
  ENDMETHOD.                    "get_fail




ENDCLASS.
