"! <p class="shorttext synchronized" lang="en">JSON Utility Class</p>
CLASS zcl_json_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA gr_array_index_matcher TYPE REF TO cl_abap_regex READ-ONLY .

    CLASS-METHODS get
      IMPORTING
        !ir_json_object        TYPE REF TO zif_json_element
        !iv_path               TYPE string
        !iv_default            TYPE any DEFAULT ''
      RETURNING
        VALUE(rr_json_element) TYPE REF TO zif_json_element .
    "! <p class="shorttext synchronized" lang="en">Map JSON Element to ABAP Structure or Internal Table</p>
    CLASS-METHODS cast_to_abap
      IMPORTING
        !ir_json_ele TYPE REF TO zif_json_element
      CHANGING
        !c_data      TYPE REF TO data .
    CLASS-METHODS class_constructor .
    CLASS-METHODS convert_atimestamp_utc_json
      IMPORTING
        VALUE(iv_timestamp)          TYPE timestamp OPTIONAL
      RETURNING
        VALUE(rv_json_utc_timestamp) TYPE string .
    CLASS-METHODS convert_json_utc_atimestamp
      IMPORTING
        VALUE(iv_json_utc_timestamp) TYPE string
      RETURNING
        VALUE(rv_timestamp)          TYPE timestamp .


    CLASS-METHODS prettify_abap_name
      IMPORTING
        !iv_abap_name       TYPE string
      RETURNING
        VALUE(rv_json_name) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_json_util IMPLEMENTATION.


  METHOD cast_to_abap.

  ENDMETHOD.


  METHOD class_constructor.

    "matches <name>[<index>], subMatch1 -> name, subMatch2 -> index
    CREATE OBJECT gr_array_index_matcher
      EXPORTING
        pattern = '(^\D\w*)\[(\d*)\]'.

  ENDMETHOD.

  METHOD prettify_abap_name.

    DATA: tokens TYPE TABLE OF char128.
    FIELD-SYMBOLS: <token> LIKE LINE OF tokens.

    rv_json_name = iv_abap_name.

    TRANSLATE rv_json_name TO LOWER CASE.
    TRANSLATE rv_json_name USING `/_:_~_`.
    SPLIT rv_json_name AT `_` INTO TABLE tokens.
    DELETE tokens WHERE table_line IS INITIAL.
    LOOP AT tokens ASSIGNING <token> FROM 2.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO rv_json_name.


  ENDMETHOD.

  METHOD convert_atimestamp_utc_json.

    DATA: lv_date      TYPE d,
          lv_time      TYPE t,
          lv_str       TYPE string,
          lv_timestamp TYPE timestamp.

    IF iv_timestamp IS INITIAL.
      GET TIME STAMP FIELD lv_timestamp.
    ELSE.
      lv_timestamp = iv_timestamp.
    ENDIF.

    CONVERT TIME STAMP lv_timestamp TIME ZONE 'UTC' "cl_abap_tstmp=>get_system_timezone( )
        INTO DATE lv_date TIME lv_time.

    cl_abap_tstmp=>systemtstmp_syst2utc( EXPORTING syst_date = lv_date syst_time = lv_time
      IMPORTING utc_tstmp = lv_timestamp  ).

    "json utc timestamp 2014-12-20T20:35:04Z
    rv_json_utc_timestamp = |{ lv_timestamp  TIMESTAMP = ISO }Z|.

  ENDMETHOD.


  METHOD convert_json_utc_atimestamp.

    DATA: lr_matcher   TYPE REF TO cl_abap_matcher,
          lv_date      TYPE d,
          lv_time      TYPE t,
          lv_str       TYPE string,
          lv_timestamp TYPE timestamp.

    "check for json utc timestamp 2014-12-20T20:35:04.260000Z
    lr_matcher = zcl_json_primitive=>mv_regex_json_utc_timestamp->create_matcher( text = iv_json_utc_timestamp ).
    IF lr_matcher IS BOUND AND lr_matcher->match( ) IS NOT INITIAL.
      CLEAR lv_str.
      lv_str = iv_json_utc_timestamp(4) && iv_json_utc_timestamp+5(2)
                          && iv_json_utc_timestamp+8(2) &&  iv_json_utc_timestamp+11(2)
                          &&  iv_json_utc_timestamp+14(2) &&  iv_json_utc_timestamp+17(2).

      " get UTC timestamp
      MOVE lv_str TO lv_timestamp.

      " convert to system date and time
      cl_abap_tstmp=>systemtstmp_utc2syst( EXPORTING utc_tstmp = lv_timestamp
          IMPORTING syst_date = lv_date syst_time = lv_time ).

      " convert to system timestamp
      CONVERT TIME lv_time DATE lv_date INTO TIME STAMP rv_timestamp
            TIME ZONE 'UTC'.
    ENDIF.

  ENDMETHOD.


  METHOD get.

    DATA: lr_json_obj  TYPE REF TO zcl_json_object,
          lr_json_arr  TYPE REF TO zcl_json_array,
          lr_json_ele  TYPE REF TO zif_json_element,
          lt_path      TYPE STANDARD TABLE OF string,
          lv_path      TYPE string,
          lv_attr_name TYPE string,
          lv_index_str TYPE string,
          lv_index     TYPE i,
          lv_tabix     TYPE i,
          lv_path_len  TYPE i.

    DATA: lr_matcher TYPE REF TO cl_abap_matcher.

    IF ir_json_object->is_object( ) EQ abap_false.
      zcx_json_exception=>raise( iv_msg = 'Input Must be a JSON Object' ).
    ENDIF.

    SPLIT iv_path AT '.' INTO TABLE lt_path.

    lr_json_ele ?= ir_json_object.

    DESCRIBE TABLE lt_path LINES lv_path_len.

    LOOP AT lt_path INTO lv_path.

      lv_tabix = sy-tabix.
      CLEAR: lv_attr_name, lv_index_str, lv_index.

      "check if array index is referred [*]
      lr_matcher = gr_array_index_matcher->create_matcher( text = lv_path ).
      IF lr_matcher IS BOUND AND lr_matcher->match( ) IS NOT INITIAL.
        "array index found
        lv_attr_name = lr_matcher->get_submatch( 1 ).
        lv_index_str = lr_matcher->get_submatch( 2 ).
        lv_index = lv_index_str.
      ELSE.
        "attribute
        lv_attr_name = lv_path.
      ENDIF.

      IF lr_json_ele->is_object( ) EQ abap_true.

        lr_json_obj ?= lr_json_ele.
        lr_json_ele = lr_json_obj->get( lv_attr_name ).

        IF lr_json_ele IS NOT BOUND.
          EXIT.
        ENDIF.

        IF lr_json_ele->is_array( ) EQ abap_true.
          IF lv_index_str IS INITIAL.
            IF lv_tabix EQ lv_path_len.
              "last item in the path, can return the Array
              EXIT.
            ENDIF.
            zcx_json_exception=>raise( iv_msg = 'Array Index is expected :' && lv_attr_name ).
          ENDIF.

          lr_json_arr ?= lr_json_ele.
          lr_json_ele = lr_json_arr->get_by_index( lv_index ).

        ELSE.
          IF lv_index_str IS NOT INITIAL.
            zcx_json_exception=>raise( iv_msg = 'Array Index is not expected :' && lv_attr_name ).
          ENDIF.
        ENDIF.

*    ELSEIF lr_json_ele->is_array( ) EQ abap_true.
*
*      lr_json_arr ?= lr_json_ele.
*      lr_json_ele = lr_json_arr->get_by_index( lv_attr_name ).

      ELSE.
        CLEAR lr_json_ele.
        EXIT.
*      Zcx_json_exception=>raise( iv_msg = 'Could not get to the path' ).
      ENDIF.

    ENDLOOP.

    IF lr_json_ele IS NOT BOUND.
      IF iv_default IS NOT INITIAL.
        lr_json_ele = zcl_json_primitive=>create( iv_data = iv_default ).
      ELSE.
        lr_json_ele = zcl_json_null=>gc_instance.
      ENDIF.
    ENDIF.

    rr_json_element = lr_json_ele.

  ENDMETHOD.
ENDCLASS.
