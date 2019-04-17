
*----------------------------------------------------------------------*
*       CLASS abap_Unit_Testclass DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS abap_unit_testclass DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>abap_Unit_Testclass
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_JSON
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
      f_cut TYPE REF TO zcl_json.  "class under test

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.

    METHODS: deserialize_form_factor FOR TESTING.
    METHODS: deserialize_target_mapping FOR TESTING.
    METHODS: deserialize_array FOR TESTING.
    METHODS: deserialize_malformed FOR TESTING.
    METHODS: serialize_form_factor FOR TESTING.
    METHODS: serialize_table FOR TESTING.
    METHODS: serialize_numbers FOR TESTING.
    METHODS: serialize_types FOR TESTING.
    METHODS: serialize_ref FOR TESTING.
    METHODS: deserialize_ref FOR TESTING.
    METHODS: deserialize_types FOR TESTING.
    METHODS: deserialize_news FOR TESTING.
    METHODS: deserialize_dynamic_tile FOR TESTING.
ENDCLASS.       "abap_Unit_Testclass


*----------------------------------------------------------------------*
*       CLASS abap_Unit_Testclass IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS abap_unit_testclass IMPLEMENTATION.
* =========================================

  METHOD class_setup.
* ===================


  ENDMETHOD.       "class_Setup


  METHOD class_teardown.
* ======================


  ENDMETHOD.       "class_Teardown


  METHOD setup.
* =============

    CREATE OBJECT f_cut.
  ENDMETHOD.       "setup


  METHOD teardown.
* ================


  ENDMETHOD.       "teardown

  METHOD serialize_numbers.
    TYPES:
      BEGIN OF t_root,
            negative_i  TYPE i,
            positive_i  TYPE i,
            positive_n  TYPE n LENGTH 6,
        END OF t_root.

    DATA: ls_data TYPE t_root,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    ls_data-negative_i = -1.
    ls_data-positive_i = 10000.
    ls_data-positive_n = 1.

    lv_exp = '{"NEGATIVE_I":-1,"POSITIVE_I":10000,"POSITIVE_N":1}'.
    lv_act = zcl_json=>serialize( data = ls_data ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of numeric types fails' ).

    lv_exp = '{"negative_i":-1,"positive_i":10000,"positive_n":1}'.
    lv_act = zcl_json=>serialize( data = ls_data pretty_name = zcl_json=>pretty_mode-low_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Low case name prety printing fails' ).

  ENDMETHOD.                    "serialize_numbers

  METHOD serialize_types.
    TYPES:
      BEGIN OF ty_s_data,
            flag      TYPE xfeld,
            char      TYPE c LENGTH 12,
            numc      TYPE n LENGTH 8,
            string    TYPE string,
            xstring   TYPE xstring,
            integer   TYPE i,
            float     TYPE f,
            packed    TYPE p LENGTH 10 DECIMALS 6,
            hex       TYPE x LENGTH 10,
            date      TYPE d,
            time      TYPE t,
        END OF ty_s_data.

    CONSTANTS: pi TYPE p LENGTH 8 DECIMALS 14 VALUE '3.14159265358979',
               tz LIKE sy-zonlo VALUE 'CET'.

    DATA: ls_data   TYPE ty_s_data,
          ls_data2  TYPE ty_s_data,
          lv_act    TYPE string,
          lv_exp    LIKE lv_act,
          lv_tmst   TYPE timestamp VALUE '20150219142259'.

    ls_data-flag    = abap_true.
    ls_data-char    = 'TEST'.
    ls_data-numc    = 12345678.
    ls_data-string  = 'ABCDEFG'.
    ls_data-xstring = ls_data-string.
    ls_data-integer = 42.
    ls_data-float   = pi.
    ls_data-packed  = pi.
    ls_data-hex     = 987654321.

    CONVERT TIME STAMP lv_tmst TIME ZONE tz INTO DATE ls_data-date TIME ls_data-time.

    lv_exp    = '{"FLAG":true,"CHAR":"TEST","NUMC":12345678,"STRING":"ABCDEFG","XSTRING":"q83v","INTEGER":42,"FLOAT":3.1415926535897900E+00,"PACKED":3.141593,"HEX":"AAAAAAAAOt5osQ==","DATE":"2015-02-19","TIME":"15:22:59"}'.
    lv_act    = zcl_json=>serialize( data = ls_data ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of data types fails' ).

    zcl_json=>deserialize( EXPORTING json = lv_act CHANGING data = ls_data2 ).
    cl_aunit_assert=>assert_equals( act = ls_data2 exp = ls_data msg = 'Deserialization of data types fails' ).

  ENDMETHOD.                    "serialize_types

  METHOD serialize_form_factor.
    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE boolean,
            phone       TYPE abap_bool,
        END OF t_form_factor,
      BEGIN OF t_form_factors,
            app_default  TYPE boole_d,
            manual      TYPE t_form_factor,
        END OF t_form_factors,
      BEGIN OF t_root,
            form_factors  TYPE t_form_factors,
        END OF t_root.

    DATA: ls_data TYPE t_root,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    ls_data-form_factors-app_default    = abap_true.
    ls_data-form_factors-manual-desktop = abap_false.
    ls_data-form_factors-manual-tablet  = abap_true.
    ls_data-form_factors-manual-phone   = abap_true.

    lv_exp = '{"formFactors":{"appDefault":true,"manual":{"desktop":false,"tablet":true,"phone":true}}}'.
    lv_act = zcl_json=>serialize( data = ls_data pretty_name = zcl_json=>pretty_mode-camel_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of form factor structure fails' ).

    lv_exp = '{"formFactors":{"appDefault":true,"manual":{"tablet":true,"phone":true}}}'.
    lv_act = zcl_json=>serialize( data = ls_data pretty_name = zcl_json=>pretty_mode-camel_case compress = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of form factor structure with compression fails' ).

    lv_exp = '{"FORM_FACTORS":{"APP_DEFAULT":true,"MANUAL":{"TABLET":true,"PHONE":true}}}'.
    lv_act = zcl_json=>serialize( data = ls_data pretty_name = abap_false compress = abap_true ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of form factor structure with NO PRETTY NAME fails' ).

  ENDMETHOD.                    "serialize_form_factor

  METHOD serialize_table.

    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE boolean,
            phone       TYPE boole_d,
        END OF t_form_factor,
      BEGIN OF t_line,
          index     TYPE i,
          user      LIKE sy-uname,
          client    LIKE sy-mandt,
          ff        TYPE t_form_factor,
          strings   TYPE string_table.
    INCLUDE   TYPE t_form_factor.
    TYPES: END OF t_line .
    TYPES: t_table TYPE HASHED TABLE OF t_line WITH UNIQUE KEY index.

    DATA: lt_data TYPE t_table,
          ls_data LIKE LINE OF lt_data,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    ls_data-index       = 1.
    ls_data-user        = 'USER1'.
    ls_data-client      = '000'.
    ls_data-ff-desktop  = abap_false.
    ls_data-ff-tablet   = abap_true.
    ls_data-ff-phone    = abap_false.
    ls_data-desktop     = abap_true.
    ls_data-tablet      = abap_false.
    ls_data-phone       = abap_true.

    APPEND 'ABC' TO ls_data-strings.
    APPEND 'BCD' TO ls_data-strings.

    INSERT ls_data INTO TABLE lt_data.

    CLEAR: ls_data.

    ls_data-index       = 2.
    ls_data-user        = 'USER2'.
    ls_data-client      = '111'.
    ls_data-ff-desktop  = abap_true.
    ls_data-ff-tablet   = abap_true.
    ls_data-ff-phone    = abap_false.
    ls_data-desktop     = abap_false.
    ls_data-tablet      = abap_false.
    ls_data-phone       = abap_true.

    APPEND 'DEF' TO ls_data-strings.

    INSERT ls_data INTO TABLE lt_data.

    CONCATENATE `[{"index":1,"user":"USER1","client":"000","ff":{"desktop":false,"tablet":true,"phone":false},"strings":["ABC","BCD"],"desktop":true,"tablet":false,"phone":true},`
                `{"index":2,"user":"USER2","client":"111","ff":{"desktop":true,"tablet":true,"phone":false},"strings":["DEF"],"desktop":false,"tablet":false,"phone":true}]`
                INTO lv_exp.

    lv_act = zcl_json=>serialize( data = lt_data pretty_name = zcl_json=>pretty_mode-camel_case ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of the table in JSON fails' ).

  ENDMETHOD.                    "serialize_table

  METHOD serialize_ref.

    DATA: lt_data TYPE abap_parmbind_tab,
          ls_data LIKE LINE OF lt_data,
          lv_int  TYPE int4,
          lv_act  TYPE string,
          lv_exp  LIKE lv_act.

    ls_data-name = 'INTERGER'.
    ls_data-kind = 'E'.
    lv_int = 3.
    GET REFERENCE OF lv_int INTO ls_data-value.
    INSERT ls_data INTO TABLE lt_data.

    lv_exp = `[{"NAME":"INTERGER","KIND":"E","VALUE":3}]`.
    lv_act = zcl_json=>serialize( data = lt_data ).

*    DATA: xml  TYPE string.
*    CALL TRANSFORMATION id OPTIONS data_refs = 'embedded'
*                           SOURCE lt_data = lt_data
*                           RESULT XML xml.                  "#EC NOTEXT

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Serialization of data reference fails' ).

  ENDMETHOD.                    "serialize_ref

  METHOD deserialize_ref.

    DATA: lv_data TYPE string,
          lt_act  TYPE abap_parmbind_tab,
          ls_act  LIKE LINE OF lt_act,
          lv_int  TYPE int4,
          lv_str  TYPE string,
          lv_bool TYPE abap_bool,
          ls_data LIKE LINE OF lt_act,
          lt_exp  LIKE lt_act,
          ls_exp  LIKE LINE OF lt_exp.

    ls_data-name = 'INTERGER'.
    ls_data-kind = 'E'.
    lv_int = 3.
    GET REFERENCE OF lv_int INTO ls_data-value.
    INSERT ls_data INTO TABLE lt_exp.

    ls_data-name = 'STRING'.
    ls_data-kind = 'E'.
    lv_str = 'Test'.
    GET REFERENCE OF lv_str INTO ls_data-value.
    INSERT ls_data INTO TABLE lt_exp.

    ls_data-name = 'BOOL'.
    ls_data-kind = 'E'.
    lv_bool = abap_true.
    GET REFERENCE OF lv_bool INTO ls_data-value.
    INSERT ls_data INTO TABLE lt_exp.

    lv_data = `[{"NAME":"INTERGER","KIND":"E","VALUE":3},{"NAME":"STRING","KIND":"E","VALUE":"Test"},{"NAME":"BOOL","KIND":"E","VALUE":true}]`.
    zcl_json=>deserialize( EXPORTING json = lv_data CHANGING data = lt_act ).

    READ TABLE lt_act INTO ls_act WITH TABLE KEY name = 'INTERGER'.
    READ TABLE lt_exp INTO ls_exp WITH TABLE KEY name = 'INTERGER'.

    cl_aunit_assert=>assert_equals( act = ls_act-name exp = ls_exp-name msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-kind exp = ls_exp-kind msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-value exp = ls_exp-value msg = 'Serialization of data reference fails' ).

    READ TABLE lt_act INTO ls_act WITH TABLE KEY name = 'STRING'.
    READ TABLE lt_exp INTO ls_exp WITH TABLE KEY name = 'STRING'.

    cl_aunit_assert=>assert_equals( act = ls_act-name exp = ls_exp-name msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-kind exp = ls_exp-kind msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-value exp = ls_exp-value msg = 'Serialization of data reference fails' ).

    READ TABLE lt_act INTO ls_act WITH TABLE KEY name = 'BOOL'.
    READ TABLE lt_exp INTO ls_exp WITH TABLE KEY name = 'BOOL'.

    cl_aunit_assert=>assert_equals( act = ls_act-name exp = ls_exp-name msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-kind exp = ls_exp-kind msg = 'Serialization of data reference fails' ).
    cl_aunit_assert=>assert_equals( act = ls_act-value exp = ls_exp-value msg = 'Serialization of data reference fails' ).


  ENDMETHOD.                    "deserialize_ref

  METHOD deserialize_form_factor.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE abap_bool,
            phone       TYPE abap_bool,
        END OF t_form_factor,
      BEGIN OF t_form_factors,
            app_default  TYPE abap_bool,
            manual      TYPE t_form_factor,
        END OF t_form_factors,
      BEGIN OF t_root,
            form_factors  TYPE t_form_factors,
        END OF t_root.

    DATA: lv_act TYPE t_root,
          lv_exp LIKE lv_act.

    lv_exp-form_factors-app_default    = abap_true.
    lv_exp-form_factors-manual-desktop = abap_false.
    lv_exp-form_factors-manual-tablet  = abap_true.
    lv_exp-form_factors-manual-phone   = abap_true.

    CONCATENATE '{ "formFactors": {'            cl_abap_char_utilities=>cr_lf
                '    "appDefault" :  true,'     cl_abap_char_utilities=>cr_lf
                '    "manual": {'               cl_abap_char_utilities=>cr_lf
                '         "desktop": false,'    cl_abap_char_utilities=>cr_lf
                '         "tablet": true,'      cl_abap_char_utilities=>cr_lf
                '         "phone": true'        cl_abap_char_utilities=>cr_lf
                '         }'                    cl_abap_char_utilities=>cr_lf
                '    }'                         cl_abap_char_utilities=>cr_lf
                '}' INTO json.

    zcl_json=>deserialize( EXPORTING json = json pretty_name = zcl_json=>pretty_mode-camel_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON fails' ).

  ENDMETHOD.       "deserialize_form_factor

  METHOD deserialize_malformed.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF tp_s_rating,
        question_id TYPE string,
        value TYPE i,
      END OF tp_s_rating,
      tp_t_rating TYPE STANDARD TABLE OF tp_s_rating WITH KEY question_id.

    DATA: lv_act TYPE tp_t_rating.

    TRY .
        json = `{"text": "x's feedback","ratings": [{"question_id":"Q1","value":3},{"question_id":"Q2","value":4},{"question_id":"Q3","value":6}`.
        zcl_json=>deserialize( EXPORTING json = json CHANGING data = lv_act ).
      CATCH cx_sy_move_cast_error.
        CLEAR lv_act.
      CATCH cx_root.                                     "#EC CATCH_ALL
        CLEAR lv_act.
    ENDTRY.

    cl_aunit_assert=>assert_initial( act = lv_act msg = 'Deserialization of wrong JSON object fails' ).

    TRY.
        json = '{ "userName": "sap", "password": "123456" }'.
        DATA: BEGIN OF user,
                username TYPE string,
                password TYPE int4,
              END OF user.
        zcl_json=>deserialize( EXPORTING json = json CHANGING data = user ).
      CATCH cx_sy_move_cast_error.                      "#EC NO_HANDLER
    ENDTRY.

    cl_aunit_assert=>assert_equals( act = user-username exp = 'sap'  msg = 'Deserialization of wrong JSON object fails' ).
    cl_aunit_assert=>assert_equals( act = user-password exp = 123456 msg = 'Deserialization of wrong JSON object fails' ).

  ENDMETHOD.                    "deserialize_malformed


  METHOD deserialize_target_mapping.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE abap_bool,
            phone       TYPE abap_bool,
        END OF t_form_factor,
      BEGIN OF t_form_factors,
            app_default  TYPE abap_bool,
            manual      TYPE t_form_factor,
        END OF t_form_factors,
      BEGIN OF t_tm_config,
            semantic_object               TYPE string,
            semantic_action               TYPE string,
            navigation_provider           TYPE string,
            navigation_provider_role      TYPE string,
            navigation_provider_instance  TYPE string,
            target_application_alias      TYPE string,
            mapping_signature             TYPE string,
            display_info_text             TYPE string,
            form_factors                  TYPE t_form_factors,
        END OF t_tm_config,
      BEGIN OF t_config,
            tile_configuration  TYPE string,
        END OF t_config.

    DATA: lv_temp TYPE t_config,
          lv_act  TYPE t_tm_config,
          lv_exp  LIKE lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-semantic_object              = 'SalesOrder'.
    lv_exp-semantic_action              = 'showFactsheet'.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = 'UI3_SRVC'.
    lv_exp-navigation_provider_instance = 'UI2_FIORI_CHECKS'.
    lv_exp-target_application_alias     = 'FactsheetApp'.
    lv_exp-display_info_text            = ''.

    CONCATENATE '{"tileConfiguration":"{\"semantic_object\":\"SalesOrder\",\"semantic_action\":\"showFactsheet\",\"navigation_provider\":\"LPD\",\"navigation_provider_role\":\"UI3_SRVC\",\"navigation_provider_instance\":\"UI2_FIORI_CHECKS\",'
                '\"target_application_alias\":\"FactsheetApp\",\"unknown\":100.00,\"display_info_text\":\"\"}"}' INTO json.

    zcl_json=>deserialize( EXPORTING json = json pretty_name = zcl_json=>pretty_mode-camel_case CHANGING data = lv_temp ).
    zcl_json=>deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = zcl_json=>pretty_mode-camel_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON fails' ).

**********************************************************************

    CLEAR lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-semantic_object              = ''.
    lv_exp-semantic_action              = ''.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = ''.
    lv_exp-navigation_provider_instance = ''.
    lv_exp-target_application_alias     = ''.
    lv_exp-display_info_text            = ''.
    lv_exp-mapping_signature            = '{par1=vallkl}&[par2=Eyallk]'.
    lv_exp-form_factors-app_default    = abap_true.
    lv_exp-form_factors-manual-desktop = abap_true.
    lv_exp-form_factors-manual-tablet  = abap_true.
    lv_exp-form_factors-manual-phone   = abap_true.

    CONCATENATE  '{"tileConfiguration":"{\"semantic_object\":\"\",\"semantic_action\":\"\",\"navigation_provider\":\"LPD\",\"display_info_text\":\"\",\"form_factors\":{\"appDefault\":true,\"manual\":'
                 '{\"desktop\":true,\"tablet\":true,\"phone\":true}},\"mapping_signature\":\"{par1=vallkl}&[par2=Eyallk]\",\"rows\":[{\"mandatory\":true,\"defaultValue\":\"\",\"isRegularExpression\":true,'
                 '\"name\":\"par1\",\"value\":\"vallkl\",\"valEnabled\":true,\"defValEnabled\":false},{\"mandatory\":false,\"isRegularExpression\":false,\"value\":\"\",\"name\":\"par2\",\"defaultValue\":'
                 '\"Eyallk\",\"valEnabled\":false,\"defValEnabled\":true}]}"}' INTO json.

    zcl_json=>deserialize( EXPORTING json = json pretty_name = zcl_json=>pretty_mode-camel_case CHANGING data = lv_temp ).
    zcl_json=>deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = zcl_json=>pretty_mode-camel_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON with array fails' ).

**********************************************************************
    CLEAR lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-semantic_object              = 'Action'.
    lv_exp-semantic_action              = 'toUrlOnOtherServer'.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = 'FLP_SAMPLE'.
    lv_exp-navigation_provider_instance = 'UI_INTEGRATION_SAMPLES'.
    lv_exp-target_application_alias     = 'toUrlOnOtherServer'.
    lv_exp-display_info_text            = '"Manage Products" app on another server'.
    lv_exp-mapping_signature            = '*=*'.
    lv_exp-form_factors-app_default    = abap_false.
    lv_exp-form_factors-manual-desktop = abap_true.
    lv_exp-form_factors-manual-tablet  = abap_true.
    lv_exp-form_factors-manual-phone   = abap_true.

    CONCATENATE  '{"tileConfiguration":"{\"semantic_object\":\"Action\",\"semantic_action\":\"toUrlOnOtherServer\",'
                 '\"display_title_text\":\"\",\"url\":\"\",\"ui5_component\":\"\",\"navigation_provider\":\"LPD\",'
                 '\"navigation_provider_role\":\"FLP_SAMPLE\",\"navigation_provider_instance\":\"UI_INTEGRATION_SAMPLES\",'
                 '\"target_application_id\":\"\",\"target_application_alias\":\"toUrlOnOtherServer\",'
                 '\"display_info_text\":\"\\\"Manage Products\\\" app on another server\",\"mapping_signature\":\"*=*\"}"}' INTO json.

    TRY.
        zcl_json=>deserialize( EXPORTING json = json pretty_name = zcl_json=>pretty_mode-camel_case CHANGING data = lv_temp ).
        zcl_json=>deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = zcl_json=>pretty_mode-camel_case CHANGING data = lv_act ).
      CATCH cx_sy_move_cast_error. " JSON structure is invalid
        CLEAR lv_act.
    ENDTRY.

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON with array fails' ).

  ENDMETHOD.       "deserialize_target_mapping

  METHOD deserialize_array.

    DATA: json TYPE string.

    TYPES:
      BEGIN OF t_form_factor,
            desktop     TYPE abap_bool,
            tablet      TYPE abap_bool,
            phone       TYPE abap_bool,
        END OF t_form_factor,
      BEGIN OF t_form_factors,
            app_default  TYPE abap_bool,
            manual      TYPE t_form_factor,
        END OF t_form_factors,
      BEGIN OF tp_s_sig_param,
            name                  TYPE string,
            value                 TYPE string,
            default_value         TYPE string,
            mandatory             TYPE abap_bool,
            isregularexpression   TYPE abap_bool,
            val_enabled           TYPE abap_bool,
            def_val_enabled       TYPE abap_bool,
        END OF tp_s_sig_param,
      tp_t_sig_param TYPE SORTED TABLE OF tp_s_sig_param WITH NON-UNIQUE KEY name,
      BEGIN OF t_tm_config,
            semantic_object               TYPE string,
            semantic_action               TYPE string,
            navigation_provider           TYPE string,
            navigation_provider_role      TYPE string,
            navigation_provider_instance  TYPE string,
            target_application_alias      TYPE string,
            mapping_signature             TYPE string,
            display_info_text             TYPE string,
            rows                          TYPE tp_t_sig_param,
            form_factors                  TYPE t_form_factors,
        END OF t_tm_config,
      BEGIN OF t_config,
            tile_configuration  TYPE string,
        END OF t_config.

    DATA: lv_temp TYPE t_config,
          lv_act  TYPE t_tm_config,
          ls_row  TYPE LINE OF tp_t_sig_param,
          lv_exp  LIKE lv_act.

    lv_act-form_factors-app_default    = abap_false.
    lv_act-form_factors-manual-desktop = abap_true.
    lv_act-form_factors-manual-tablet  = abap_true.
    lv_act-form_factors-manual-phone   = abap_true.
    lv_act-display_info_text           = 'default text'.

    lv_exp = lv_act.
    lv_exp-form_factors-app_default     = abap_true.
    lv_exp-semantic_object              = 'SalesOrder'.
    lv_exp-semantic_action              = 'showFactsheet'.
    lv_exp-navigation_provider          = 'LPD'.
    lv_exp-navigation_provider_role     = 'UI3_SRVC'.
    lv_exp-navigation_provider_instance = 'UI2_FIORI_CHECKS'.
    lv_exp-target_application_alias     = 'FactsheetApp'.
    lv_exp-display_info_text            = ''.
    lv_exp-mapping_signature            = '{par1=vallkl}&[par2=Eyallk]'.

    ls_row-name                  = 'par1'.
    ls_row-value                 = 'vallkl'.
    ls_row-default_value         = ''.
    ls_row-mandatory             = abap_true.
    ls_row-isregularexpression   = abap_true.
    ls_row-val_enabled           = abap_true.
    ls_row-def_val_enabled       = abap_false.
    INSERT ls_row INTO TABLE lv_exp-rows.

    ls_row-name                  = 'par2'.
    ls_row-value                 = ''.
    ls_row-default_value         = 'Eyallk'.
    ls_row-mandatory             = abap_false.
    ls_row-isregularexpression   = abap_false.
    ls_row-val_enabled           = abap_false.
    ls_row-def_val_enabled       = abap_true.
    INSERT ls_row INTO TABLE lv_exp-rows.

    CONCATENATE  '{"tileConfiguration":"{\"semantic_object\":\"SalesOrder\",\"semantic_action\":\"showFactsheet\",\"navigation_provider\":\"LPD\",\"display_info_text\":\"\",\"form_factors\":{\"appDefault\":true,\"manual\":'
                 '{\"desktop\":true,\"tablet\":true,\"phone\":true}},\"mapping_signature\":\"{par1=vallkl}&[par2=Eyallk]\",\"rows\":[{\"mandatory\":true,\"defaultValue\":\"\",\"isRegularExpression\":true,'
                 '\"name\":\"par1\",\"value\":\"vallkl\",\"valEnabled\":true,\"defValEnabled\":false},{\"mandatory\":false,\"isRegularExpression\":false,\"value\":\"\",\"name\":\"par2\",\"defaultValue\":'
                 '\"Eyallk\",\"valEnabled\":false,\"defValEnabled\":true}],'
                 '\"target_application_alias\":\"FactsheetApp\",\"navigation_provider_role\":\"UI3_SRVC\",\"navigation_provider_instance\":\"UI2_FIORI_CHECKS\" }"}' INTO json.

    zcl_json=>deserialize( EXPORTING json = json pretty_name = zcl_json=>pretty_mode-camel_case CHANGING data = lv_temp ).
    zcl_json=>deserialize( EXPORTING json = lv_temp-tile_configuration pretty_name = zcl_json=>pretty_mode-camel_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialization of JSON with array fails' ).

  ENDMETHOD.       "deserialize_target_mapping_array

  METHOD deserialize_types.
    TYPES:
      BEGIN OF t_struct,
            negative_i  TYPE i,
            positive_i  TYPE i,
            positive_n  TYPE n LENGTH 6,
            timestamp   TYPE timestamp,
            boolean     TYPE abap_bool,
        END OF t_struct.

    DATA: lv_exp  TYPE t_struct,
          lv_act  TYPE t_struct,
          lv_data TYPE string.

    lv_exp-negative_i = -1.
    lv_exp-positive_i = 10000.
    lv_exp-positive_n = 1.
    lv_exp-timestamp  = 1419279663821.
    lv_exp-boolean    = abap_true.

    lv_data = '{"negative_i": -1, "positive_i":10000, "positive_n" : 1, "boolean" : true, "timestamp" : 1419279663821, "timestamp_not_mapped" : 1419279663821}'.
    zcl_json=>deserialize( EXPORTING json = lv_data pretty_name = zcl_json=>pretty_mode-low_case CHANGING data = lv_act ).

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialize of types with low case name prety printing fails' ).

  ENDMETHOD.                    "deserialize_types

  METHOD deserialize_news.
    TYPES: BEGIN OF tp_s_tile_news_config,
            defaultimage                   TYPE string,
            cycleinterval                  TYPE i,
            refreshinterval                TYPE string,
            usedefaultimage                TYPE abap_bool,
            feed1                          TYPE string,
            feed2                          TYPE string,
            feed3                          TYPE string,
            feed4                          TYPE string,
            feed5                          TYPE string,
            feed6                          TYPE string,
            feed7                          TYPE string,
            feed8                          TYPE string,
            feed9                          TYPE string,
            feed10                         TYPE string,
            ifilter1                       TYPE string,
            ifilter2                       TYPE string,
            ifilter3                       TYPE string,
            ifilter4                       TYPE string,
            ifilter5                       TYPE string,
            efilter1                       TYPE string,
            efilter2                       TYPE string,
            efilter3                       TYPE string,
            efilter4                       TYPE string,
            efilter5                       TYPE string,
           END OF tp_s_tile_news_config .

    DATA: lv_exp  TYPE tp_s_tile_news_config,
          lv_act  TYPE tp_s_tile_news_config,
          lv_data TYPE string.

    CONCATENATE '{"defaultImage":"http://\\","cycleInterval":"500","refreshInterval":"15 Minutes","useDefaultImage":"false",'
                '"feed1":"","feed2":"","feed3":"","feed4":"","feed5":"","feed6":"","feed7":"","feed8":"","feed9":"","feed10":"",'
                '"iFilter1":"","iFilter2":"","iFilter3":"","iFilter4":"","iFilter5":"","eFilter1":"","eFilter2":"","eFilter3":"","eFilter4":"","eFilter5":""}'
                INTO lv_data.

    lv_exp-defaultimage     = 'http://\'.
    lv_exp-cycleinterval    = '500'.
    lv_exp-refreshinterval  = '15 Minutes'.
    lv_exp-usedefaultimage  = abap_false.

    TRY.
        zcl_json=>deserialize( EXPORTING json = lv_data pretty_name = zcl_json=>pretty_mode-low_case CHANGING data = lv_act ).
      CATCH cx_sy_move_cast_error. " JSON structure is invalid
        CLEAR lv_act.
    ENDTRY.

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialize of types with low case name prety printing fails' ).

**********************************************************************

    CONCATENATE '{"defaultImage":"http://\\\\","cycleInterval":500,"refreshInterval":"15 Minutes","useDefaultImage":"true",'
                '"feed1":"","feed2":"","feed3":"","feed4":"","feed5":"","feed6":"","feed7":"","feed8":"","feed9":"","feed10":"",'
                '"iFilter1":"","iFilter2":"","iFilter3":"","iFilter4":"","iFilter5":"","eFilter1":"","eFilter2":"","eFilter3":"","eFilter4":"","eFilter5":""}'
                INTO lv_data.

    lv_exp-defaultimage     = 'http://\\'.
    lv_exp-cycleinterval    = '500'.
    lv_exp-refreshinterval  = '15 Minutes'.
    lv_exp-usedefaultimage  = abap_true.

    TRY.
        zcl_json=>deserialize( EXPORTING json = lv_data pretty_name = zcl_json=>pretty_mode-low_case CHANGING data = lv_act ).
      CATCH cx_sy_move_cast_error. " JSON structure is invalid
        CLEAR lv_act.
    ENDTRY.

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialize of types with low case name prety printing fails' ).

**********************************************************************

  ENDMETHOD.                    "deserialize_news

  METHOD deserialize_dynamic_tile.
    TYPES:
     BEGIN OF tp_s_tile_dynamic_config,
               display_icon_url               TYPE string,
               display_info_text              TYPE string,
               display_title_text             TYPE string,
               display_subtitle_text          TYPE string,
               navigation_use_semantic_object TYPE abap_bool,
               navigation_target_url          TYPE string,
               navigation_semantic_object     TYPE string,
               navigation_semantic_action     TYPE string,
               navigation_semantic_parameters TYPE string,
               display_search_keywords        TYPE string,
               display_number_unit            TYPE string,
               service_url                    TYPE string,
               service_refresh_interval       TYPE i,
           END OF tp_s_tile_dynamic_config .

    DATA: lv_exp  TYPE tp_s_tile_dynamic_config,
          lv_act  TYPE tp_s_tile_dynamic_config,
          lv_data TYPE string.

    CONCATENATE '{"display_icon_url":"","display_title_text":"","di'
                'splay_subtitle_text":"","display_info_text":"","di'
                'splay_number_unit":"","service_url":"","service_re'
                'fresh_interval":"2343q44we5e55","navigation_use_se'
                'mantic_object":true,"navigation_target_url":"#Qual'
                'ityNotificationActivity-,..,?ghgh","navigation_sem'
                'antic_object":"QualityNotificationActivity","navig'
                'ation_semantic_action":",..,","navigation_semantic'
                '_parameters":"ghgh","display_search_keywords":""}'
    INTO lv_data.

    DATA: except TYPE REF TO cx_root.

    lv_exp-navigation_use_semantic_object = abap_true.
    lv_exp-navigation_target_url = '#QualityNotificationActivity-,..,?ghgh'.
    lv_exp-navigation_semantic_object = 'QualityNotificationActivity'.
    lv_exp-navigation_semantic_action = ',..,'.
    lv_exp-navigation_semantic_parameters = 'ghgh'.

    TRY.
        zcl_json=>deserialize( EXPORTING json = lv_data CHANGING data = lv_act ).
      CATCH cx_root INTO except.
        CLEAR lv_act.
    ENDTRY.

    cl_aunit_assert=>assert_equals( act = lv_act exp = lv_exp msg = 'Deserialize of dynamic tile fails!' ).


  ENDMETHOD.                    "deserialize_dynamic_tile


ENDCLASS.       "abap_Unit_Testclass
