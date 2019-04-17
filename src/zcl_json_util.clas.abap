class ZCL_JSON_UTIL definition
  public
  final
  create public .

public section.

  class-data GR_ARRAY_INDEX_MATCHER type ref to CL_ABAP_REGEX read-only .

  class-methods GET
    importing
      !IR_JSON_OBJECT type ref to ZIF_JSON_ELEMENT
      !IV_PATH type STRING
      !IV_DEFAULT type ANY default ''
    returning
      value(RR_JSON_ELEMENT) type ref to ZIF_JSON_ELEMENT .
  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_JSON_UTIL IMPLEMENTATION.


METHOD CLASS_CONSTRUCTOR.

  "matches <name>[<index>], subMatch1 -> name, subMatch2 -> index
  CREATE OBJECT gr_array_index_matcher
    EXPORTING
      pattern = '(^\D\w*)\[(\d*)\]'.

ENDMETHOD.


METHOD get.

  DATA: lr_json_obj TYPE REF TO zcl_json_object,
        lr_json_arr TYPE REF TO zcl_json_array,
        lr_json_ele TYPE REF TO zif_json_element,
        lt_path TYPE STANDARD TABLE OF string,
        lv_path TYPE string,
        lv_attr_name TYPE string,
        lv_index_str TYPE string,
        lv_index TYPE i,
        lv_tabix TYPE i,
        lv_path_len TYPE i.

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
*      zcx_json_exception=>raise( iv_msg = 'Could not get to the path' ).
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
