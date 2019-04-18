
CLASS ztest_json_builder DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ztest_Json_Builder
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_JSON_BUILDER
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
    DATA:
      r_builder     TYPE REF TO zcl_json_builder,  "class under test

      v_equal       TYPE abap_bool,
      v_int         TYPE i VALUE 223444889,
      v_int_str     TYPE string VALUE '223444889',
      v_nint        TYPE i VALUE -847,
      v_nint_str    TYPE string VALUE '-847',
      v_big_int     TYPE p LENGTH 12 DECIMALS 0 VALUE 9484782749,
      v_big_int_str TYPE string VALUE '9484782749',
      v_float       TYPE p LENGTH 6 DECIMALS 4 VALUE '1234567.88',
      v_float_str   TYPE string VALUE '1234567.8800',
      v_nfloat      TYPE p LENGTH 14 DECIMALS 4 VALUE '4792844.489-',
      v_nfloat_str  TYPE string VALUE '-4792844.4890',
      v_string      TYPE string VALUE 'some string goes here',
      v_string_str  TYPE string VALUE '"some string goes here"',
      v_bool        TYPE abap_bool VALUE abap_true,
      v_bool_str    TYPE string VALUE 'true',
      v_data        TYPE bu_partner VALUE '9484849382',
      v_data_str    TYPE string VALUE '"9484849382"'.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: z FOR TESTING.
ENDCLASS.       "ztest_Json_Builder


CLASS ztest_json_builder IMPLEMENTATION.

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


  METHOD z.
* ==============
* Test JSON object
*{
*   "data" : {
*     "number" : 333,
*     "bool" : true,
*     "null" : null
*     "array" : [
*       444,
*       {
*         "number" : 48,
*         "string" : "Some Text"
*       },
*       98,
*       false
*     ]
*	  }
*}

    DATA: lr_element TYPE REF TO zif_json_element.
    DATA: lv_str_act TYPE string.
    DATA lv_str_exp TYPE string.


    DATA: lr_primitive TYPE REF TO zcl_json_primitive.

    "Prepare Test Data Final JSON string - Begin
*    lr_primitive = Zcl_json_primitive=>Z( v_big_int ).
*    lv_str_act = lr_primitive->to_string( ).
*
*    CONCATENATE `{"data":{"number":` lv_str_act INTO lv_str_exp.
*    CONCATENATE lv_str_exp `,"bool":` v_bool_str `,"null":null,"array":[` INTO lv_str_exp.
*
*    lr_primitive->set_value( v_int ).
*    lv_str_act = lr_primitive->to_string( ).
*    CONCATENATE lv_str_exp lv_str_act `,{"negFloat":` INTO lv_str_exp.
*
*    lr_primitive->set_value( v_nfloat ).
*    lv_str_act = lr_primitive->to_string( ).
*    CONCATENATE lv_str_exp lv_str_act `,"string":"` v_string `"},` INTO lv_str_exp.
*
*    lr_primitive->set_value( v_nint ).
*    lv_str_act = lr_primitive->to_string( ).
*    CONCATENATE lv_str_exp lv_str_act `,` v_bool_str `]}}` INTO lv_str_exp.

    lv_str_exp = |\{"data":\{"number":{ v_big_int },"bool":{ v_bool_str },"null":null,"array":[|.
    lv_str_exp = |{ lv_str_exp }{ v_int },\{"negFloat":{ v_nfloat },"string":"{ v_string }"\}|.
    lv_str_exp = |{ lv_str_exp },{ v_nint },{ v_bool_str }]\}\}|.
    CLEAR lv_str_act.
    "Prepare Test Data Final JSON string - End

    r_builder = zcl_json_builder=>create( ). "start with object

    r_builder->start( iv_type = zif_json_element=>c_type_object iv_name = 'data' ).
    "under 'data' object
    r_builder->add( iv_name = 'number' iv_value = v_big_int ).
    r_builder->add( iv_name = 'bool' iv_value = v_bool ).
    r_builder->add( iv_name = 'null' ir_element = zcl_json_null=>gc_instance ).

    r_builder->start( iv_type = zif_json_element=>c_type_array iv_name = 'array' ).
    "under 'data->array' array
    r_builder->add( iv_value = v_int ).
    r_builder->start( iv_type = zif_json_element=>c_type_object ).

    "under 'data->array->{}' object
    r_builder->add( iv_name = 'negFloat' iv_value = v_nfloat ).
    r_builder->add( iv_name = 'string' iv_value = v_string ).
    r_builder->end( ).

    "back to 'data->array'
    r_builder->add( iv_name = 'negInt' iv_value = v_nint ).
    r_builder->add( iv_name = 'bool' iv_value = v_bool ).


    lr_element = r_builder->build( ).

    cl_abap_unit_assert=>assert_bound(
      act   = lr_element
      msg   = 'Build Failed'
    ).


    lv_str_act = lr_element->to_string( ).



    cl_abap_unit_assert=>assert_equals(
      act = lv_str_act
      exp = lv_str_exp
      msg = 'Builder Generated Wrong JSON' ).

  ENDMETHOD.       "Z

ENDCLASS.
