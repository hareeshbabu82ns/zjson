
CLASS ztest_json_parser DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ztest_Json_Parser
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_JSON_PARSER
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
      f_cut         TYPE REF TO zcl_json_parser,  "class under test

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

    DATA: v_json_str TYPE string.

    METHODS: setup.
    METHODS: teardown.
    METHODS: parse_json FOR TESTING.
ENDCLASS.       "ztest_Json_Parser


CLASS ztest_json_parser IMPLEMENTATION.
  METHOD setup.
* =============

    v_json_str = |\{"data":\{"number":{ v_big_int },"bool":{ v_bool_str },"null":null,"array":[|.
    v_json_str = |{ v_json_str }{ v_int },\{"negFloat":{ v_nfloat },"string":"{ v_string }"\}|.
    v_json_str = |{ v_json_str },{ v_nint },{ v_bool_str }]\}\}|.

  ENDMETHOD.       "setup


  METHOD teardown.
* ================


  ENDMETHOD.       "teardown

  METHOD parse_json.
* =================

    DATA: lr_json TYPE REF TO zif_json_element,
          lv_str  TYPE string.

    zcl_json_parser=>from_json( EXPORTING json = v_json_str CHANGING json_element = lr_json ).

    cl_abap_unit_assert=>assert_bound( act = lr_json msg = 'JSON Parsing Failed' ).

    zcl_json_parser=>to_json( EXPORTING json_element = lr_json RECEIVING r_json = lv_str ).

    cl_abap_unit_assert=>assert_equals( act = lv_str exp = v_json_str msg = 'JSON Conversion Mismatch').


  ENDMETHOD.       "parse_Json




ENDCLASS.
