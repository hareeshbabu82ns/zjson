
CLASS ztest_json_primitive DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>ztest_Json_Primitive
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_JSON_PRIMITIVE
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
      r_primitive     TYPE REF TO zcl_json_primitive,  "class under test
      r_prim          TYPE REF TO zcl_json_primitive,
      v_equal         TYPE abap_bool,
      v_int           TYPE i VALUE 223444889,
      v_int_str       TYPE string VALUE '223444889',
      v_nint          TYPE i VALUE -847,
      v_nint_str      TYPE string VALUE '-847',
      v_big_int       TYPE p LENGTH 12 DECIMALS 0 VALUE 9484782749,
      v_big_int_str   TYPE string VALUE '9484782749',
      v_float         TYPE p LENGTH 6 DECIMALS 4 VALUE '1234567.88',
      v_float_str     TYPE string VALUE '1234567.8800',
      v_nfloat        TYPE p LENGTH 14 DECIMALS 4 VALUE '4792844.489-',
      v_nfloat_str    TYPE string VALUE '-4792844.4890',
      v_string        TYPE string VALUE 'some string goes here',
      v_string_str    TYPE string VALUE '"some string goes here"',
      v_bool          TYPE abap_bool VALUE abap_true,
      v_bool_str      TYPE string VALUE 'true',
      v_data          TYPE bu_partner VALUE '9484849382',
      v_data_str      TYPE string VALUE '"9484849382"',
      v_timestamp     TYPE timestamp VALUE '20141220203504',
      v_timestamp_str TYPE string VALUE '"2014-12-20T20:35:04.260000Z"'.


    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: deep_copy FOR TESTING.
    METHODS: get_abap_type FOR TESTING.
    METHODS: get_type FOR TESTING.
    METHODS: to_dref FOR TESTING.
    METHODS: to_string FOR TESTING.
    METHODS: get_primitive_type FOR TESTING.
    METHODS: set_value FOR TESTING.
    METHODS: from_string FOR TESTING.
    METHODS: as_data FOR TESTING.
ENDCLASS.       "ztest_Json_Primitive


CLASS ztest_json_primitive IMPLEMENTATION.

  METHOD class_setup.
* ===================


  ENDMETHOD.       "class_Setup


  METHOD class_teardown.
* ======================


  ENDMETHOD.       "class_Teardown


  METHOD setup.
* =============
    GET TIME STAMP FIELD v_timestamp.
    v_timestamp_str = v_timestamp.
*    CREATE OBJECT f_cut.
  ENDMETHOD.       "setup


  METHOD teardown.
* ================


  ENDMETHOD.       "teardown

  METHOD as_data.

    r_prim = zcl_json_primitive=>create( v_nint ).

    DATA: lv_int     TYPE i,
          lv_int_str TYPE string.

    "get primitive as ABAP Int
    r_prim->as_data( CHANGING cv_data = lv_int ).

    cl_abap_unit_assert=>assert_equals(
      act   = v_nint
      exp   = lv_int
      msg   = 'Get As Data failed'
    ).

    "get primitive as ABAP string
    lv_int_str = r_prim->as_string( ).
    cl_abap_unit_assert=>assert_equals(
      act   = lv_int_str
      exp   = v_nint_str
      msg   = 'Get As String failed'
    ).
  ENDMETHOD.                    "as_data

  METHOD deep_copy.
* =================
*    DATA rv_element TYPE REF TO Zif_json_element.
*
*    rv_element = f_cut->Zif_json_element~deep_copy(  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = rv_element
*      exp   = rv_element          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Element'
**     level =
*    ).
  ENDMETHOD.       "deep_Copy


  METHOD get_abap_type.
* =====================
*    DATA rr_type TYPE REF TO cl_abap_typedescr.
*
*    rr_type = f_cut->Zif_json_element~get_abap_type(  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = rr_type
*      exp   = rr_type          "<--- please adapt expected value
*    " msg   = 'Testing value rr_Type'
**     level =
*    ).
  ENDMETHOD.       "get_Abap_Type


  METHOD get_type.
* ================
*    DATA rv_type TYPE int1.
*
*    rv_type = f_cut->Zif_json_element~get_type(  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = rv_type
*      exp   = rv_type          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Type'
**     level =
*    ).
  ENDMETHOD.       "get_Type


  METHOD to_dref.
* ===============
    r_prim = zcl_json_primitive=>create( v_nint ).

    r_primitive ?= r_prim->deep_copy( ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_true(
      act   = v_equal
      msg   = 'Deep Copy failed'
    ).

  ENDMETHOD.       "to_Dref

  METHOD from_string.

    r_primitive = zcl_json_primitive=>from_string( v_int_str ).

    cl_abap_unit_assert=>assert_bound(
     act = r_primitive
     msg = 'String to Int conversion failed'
     ).
*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>from_string( v_nint_str ).

    r_prim = zcl_json_primitive=>create( v_nint ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_true(
      act   = v_equal
      msg   = 'Negative Int Conversion failed'
    ).
*--------------------------------------------------------------------*

    r_primitive = zcl_json_primitive=>from_string( v_float_str ).

    r_prim = zcl_json_primitive=>create( v_float ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_true(
      act   = v_equal
      msg   = 'Float Conversion failed'
    ).
*--------------------------------------------------------------------*

    r_primitive = zcl_json_primitive=>from_string( v_nfloat_str ).

    r_prim = zcl_json_primitive=>create( v_nfloat ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_true(
      act   = v_equal
      msg   = 'Negative Float Conversion failed'
    ).
*--------------------------------------------------------------------*

    r_primitive = zcl_json_primitive=>from_string( v_big_int_str ).

    r_prim = zcl_json_primitive=>create( v_big_int ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_true(
      act   = v_equal
      msg   = 'Big Int Conversion failed'
    ).
*--------------------------------------------------------------------*

    r_primitive = zcl_json_primitive=>from_string( v_bool_str ).

    r_prim = zcl_json_primitive=>create( v_bool ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_true(
      act   = v_equal
      msg   = 'Bool Conversion failed'
    ).
*--------------------------------------------------------------------*
    DATA: lv_str TYPE string.
    MOVE v_data TO lv_str.
    r_primitive = zcl_json_primitive=>from_string( lv_str ).

    r_prim = zcl_json_primitive=>create( v_data ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_false(
      act   = v_equal
      msg   = 'Data Conversion failed'
    ).
*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>from_string( v_string ).

    r_prim = zcl_json_primitive=>create( v_string ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_true(
      act   = v_equal
      msg   = 'String Conversion failed'
    ).
*--------------------------------------------------------------------*

    r_primitive = zcl_json_primitive=>from_string( v_timestamp_str ).

    r_prim = zcl_json_primitive=>create( v_timestamp ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_true(
      act   = v_equal
      msg   = 'Timestamp Conversion failed'
    ).

    DATA: ts TYPE timestamp.

    v_timestamp = '20190426034852'.
    v_timestamp_str = '2019-04-26T01:48:52.260000Z'.

    GET TIME STAMP FIELD v_timestamp.

    cl_abap_tstmp=>systemtstmp_syst2utc( EXPORTING syst_date = sy-datum syst_time = sy-uzeit
        IMPORTING utc_tstmp = ts  ).

    v_timestamp_str = |{ ts  TIMESTAMP = ISO }Z| .

    r_primitive = zcl_json_primitive=>from_string( v_timestamp_str ).

    r_prim = zcl_json_primitive=>create( v_timestamp ).

    v_equal = r_prim->equals( ir_element = r_primitive ).

    cl_abap_unit_assert=>assert_true(
      act   = v_equal
      msg   = 'Timestamp Conversion failed'
    ).
*--------------------------------------------------------------------*
  ENDMETHOD.                    "from_string

  METHOD to_string.
* =================
    DATA ir_stream TYPE REF TO cl_abap_string_c_writer.

    DATA: rv_string   TYPE string,
          rv_expected TYPE string.

    r_primitive = zcl_json_primitive=>create( v_int ).

    rv_string = r_primitive->to_string( ).

    CONDENSE rv_string.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = v_int_str
      msg   = 'Int Conversion failed'
    ).
*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>create( v_nint ).

    rv_string = r_primitive->to_string( ).

    CONDENSE rv_string.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = v_nint_str
      msg   = 'Negative Int Conversion failed'
    ).

*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>create( v_big_int ).

    rv_string = r_primitive->to_string( ).

    CONDENSE rv_string.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = v_big_int_str
      msg   = 'Big Int Conversion failed'
    ).

*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>create( v_float ).

    rv_string = r_primitive->to_string( ).

    CONDENSE rv_string.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = v_float_str
      msg   = 'Float Conversion failed'
    ).
*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>create( v_nfloat ).

    rv_string = r_primitive->to_string( ).

    CONDENSE rv_string.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = v_nfloat_str
      msg   = 'Negative Float Conversion failed'
    ).

*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>create( v_bool ).

    rv_string = r_primitive->to_string( ).

    CONDENSE rv_string.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = v_bool_str
      msg   = 'Boolean Conversion failed'
    ).

*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>create( v_data ).

    rv_string = r_primitive->to_string( ).

    CONDENSE rv_string.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = v_data_str
      msg   = 'Data Conversion failed'
    ).

*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>create( v_string ).

    rv_string = r_primitive->to_string( ).

    CONDENSE rv_string.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = v_string_str
      msg   = 'String Conversion failed'
    ).

*--------------------------------------------------------------------*
*---Convert String with NewLine to JSON String---*
    DATA(lv_str_crlf) = |test1 \n test2|.
    DATA(lv_str_crlf_res) = '"test1 \n test2"'.

    r_primitive = zcl_json_primitive=>create( lv_str_crlf ).

    rv_string = r_primitive->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = lv_str_crlf_res
      msg   = 'String Conversion failed'
    ).

*--------------------------------------------------------------------*
    DATA: lr_null TYPE REF TO zif_json_element.

    lr_null = zcl_json_null=>get_instance( ).

    rv_string = lr_null->to_string( ).

    CONDENSE rv_string.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = 'null'
      msg   = 'Null Conversion failed'
    ).

*--------------------------------------------------------------------*
    r_primitive = zcl_json_primitive=>create( v_timestamp ).

    rv_string = r_primitive->to_string( ).

    CONDENSE rv_string.
    CONDENSE v_timestamp_str.

    cl_abap_unit_assert=>assert_equals(
      act   = rv_string
      exp   = v_timestamp_str
      msg   = 'Time Stamp Conversion failed'
    ).

  ENDMETHOD.       "to_String


  METHOD get_primitive_type.
* ==========================
*    DATA rv_kind TYPE int1.
*
*    rv_kind = f_cut->get_primitive_type(  ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = rv_kind
*      exp   = rv_kind          "<--- please adapt expected value
*    " msg   = 'Testing value rv_Kind'
**     level =
*    ).
  ENDMETHOD.       "get_Primitive_Type


  METHOD set_value.
* =================
    DATA: ls_bp TYPE TABLE OF but000. "structure

    TRY.
        r_primitive = zcl_json_primitive=>create( ls_bp ).

        cl_abap_unit_assert=>fail( 'Missing Exception on Non Primitive Type Creation').
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.       "set_Value







ENDCLASS.
