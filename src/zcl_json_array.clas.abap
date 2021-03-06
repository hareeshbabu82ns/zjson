class ZCL_JSON_ARRAY definition
  public
  inheriting from ZCL_JSON_ELEMENT
  final
  create public .

public section.
  type-pools ABAP .

  data MT_ELEMENTS type ZJSON_ELEMENTS_TAB read-only .

  methods ADD
    importing
      !IV_VALUE type ANY optional
      !IR_ELEMENT type ref to ZIF_JSON_ELEMENT optional .
  methods REMOVE
    importing
      !IV_INDEX type I optional
      !IR_ELEMENT type ref to ZIF_JSON_ELEMENT optional
    returning
      value(RV_REMOVED) type ABAP_BOOL .
  methods HAS
    importing
      !IR_ELEMENT type ref to ZIF_JSON_ELEMENT
    returning
      value(RV_EXIST) type ABAP_BOOL .
  methods GET
    importing
      !IV_INDEX type I
    returning
      value(RR_ELEMENT) type ref to ZIF_JSON_ELEMENT .
  methods CONSTRUCTOR
    importing
      !IT_DATA type ANY TABLE optional
      value(IT_NAMES_MAP) type ZJSON_NAME_VALUE_TAB optional
      value(IV_MAP_IGNORE_REST) type ABAP_BOOL default ABAP_FALSE .
  methods LENGTH
    returning
      value(RV_LENGTH) type INT4 .
  methods FIND
    importing
      !IR_CONDITION type ref to ZIF_JSON_ELEMENT
    returning
      value(RR_RESULT) type ref to ZIF_JSON_ELEMENT .

  methods ZIF_JSON_ELEMENT~AS_DATA
    redefinition .
  methods ZIF_JSON_ELEMENT~DEEP_COPY
    redefinition .
  methods ZIF_JSON_ELEMENT~EQUALS
    redefinition .
  methods ZIF_JSON_ELEMENT~GET_ABAP_TYPE
    redefinition .
  methods ZIF_JSON_ELEMENT~GET_TYPE
    redefinition .
  methods ZIF_JSON_ELEMENT~IS_ARRAY
    redefinition .
  methods ZIF_JSON_ELEMENT~TO_DREF
    redefinition .
  methods ZIF_JSON_ELEMENT~TO_STRING
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_BY_INDEX
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_CURRENT
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_FIRST
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_LAST
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_NEXT
    redefinition .
  methods ZIF_JSON_ITERATOR~GET_PREVIOUS
    redefinition .
  methods ZIF_JSON_ITERATOR~SIZE
    redefinition .
  methods ZIF_JSON_ELEMENT~AS_STRING
    redefinition .
protected section.
private section.

  data MR_TABLE type ref to DATA .
  data MR_TABLEDESCR type ref to CL_ABAP_TABLEDESCR .
ENDCLASS.



CLASS ZCL_JSON_ARRAY IMPLEMENTATION.


METHOD add.

  DATA: lr_element TYPE REF TO Zif_json_element,
        lr_array TYPE REF TO Zcl_json_array.

  IF ir_element IS BOUND.
    lr_element = ir_element.
  ELSEIF iv_value IS SUPPLIED.
    CREATE OBJECT lr_element TYPE Zcl_json_primitive
      EXPORTING
        ir_value = iv_value.
  ELSE.
    lr_element = Zcl_json_null=>get_instance( ).
  ENDIF.

  IF lr_element IS BOUND.
    IF lr_element->is_array( ) EQ abap_true.
      lr_array ?= ir_element.
      APPEND LINES OF lr_array->mt_elements TO mt_elements.
    ELSE.
      APPEND lr_element TO mt_elements.
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD constructor.
  super->constructor( ).

  DATA:
    lr_descr TYPE REF TO cl_abap_typedescr,
    lr_ele   TYPE REF TO zif_json_element.

  FIELD-SYMBOLS:
      <fs_line> TYPE any.

  IF it_data IS SUPPLIED AND it_data IS NOT INITIAL.
    TRY.
        mr_tabledescr ?= cl_abap_tabledescr=>describe_by_data( it_data ).
        GET REFERENCE OF it_data INTO mr_table.

        LOOP AT it_data ASSIGNING <fs_line>.

          TRY .
              lr_descr ?= cl_abap_typedescr=>describe_by_data( <fs_line> ).

              CASE lr_descr->kind.

                WHEN cl_abap_typedescr=>kind_struct.
                  CREATE OBJECT lr_ele TYPE zcl_json_object
                    EXPORTING
                      is_data            = <fs_line>
                      it_names_map       = it_names_map
                      iv_map_ignore_rest = iv_map_ignore_rest.

                  add( ir_element = lr_ele ).

                WHEN cl_abap_typedescr=>kind_elem.

                  add( iv_value = <fs_line> ).

                WHEN OTHERS.
                  "Not a Valid Table Line Type
                  zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Table Line' ).
              ENDCASE.

            CATCH cx_root..
              "Not a Valid Table Line Type
              zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Table Line' ).
          ENDTRY.

        ENDLOOP.

      CATCH cx_root.
        "Not a Valid Table Type
        zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Table' ).
    ENDTRY.
  ENDIF.

ENDMETHOD.


  METHOD find.
    DATA: lv_index   TYPE i,
          lv_found   TYPE abap_bool VALUE abap_false,
          lr_obj_src TYPE REF TO zcl_json_object,
          lr_obj_chk TYPE REF TO zcl_json_object.

    IF ir_condition IS NOT BOUND.
      zcx_json_exception=>raise( iv_msg = 'Empty Condition for Array Find' ).
    ENDIF.

    IF ir_condition->is_object( ) EQ abap_true.
      lr_obj_chk ?= ir_condition.

      LOOP AT mt_elements INTO DATA(lr_element).
        CHECK lr_element->is_object( ) EQ abap_true.
        lr_obj_src ?= lr_element.

        lv_found = abap_false.
        "compare object values partial
        LOOP AT lr_obj_chk->mt_members ASSIGNING FIELD-SYMBOL(<fr_member_chk>).

          READ TABLE lr_obj_src->mt_members ASSIGNING FIELD-SYMBOL(<fr_member_src>)
             WITH KEY name = <fr_member_chk>-name.
          IF sy-subrc IS NOT INITIAL. "no member with the name found in src object, exit
            lv_found = abap_false.
            EXIT.
          ENDIF.

          IF <fr_member_chk>-value->equals( ir_element = <fr_member_src>-value ) EQ abap_true.
            lv_found = abap_true.
          ELSE.
            lv_found = abap_false.
            EXIT.
          ENDIF.

        ENDLOOP.

        IF lv_found EQ abap_true. "exit on first match
          rr_result = lr_element.
          EXIT.
        ENDIF.

      ENDLOOP.

    ELSEIF ir_condition->is_number( ) EQ abap_true.
      ir_condition->as_data( CHANGING cv_data = lv_index ).
      rr_result = get_by_index( iv_index =  lv_index ).
    ELSE.
      zcx_json_exception=>raise( iv_msg = |Unsupported Array Find argument| ).
    ENDIF.
  ENDMETHOD.


METHOD get.
  READ TABLE mt_elements INDEX iv_index INTO rr_element.
ENDMETHOD.


METHOD HAS.

  rv_exist = abap_false.

  DATA: lr_element LIKE LINE OF mt_elements.

  LOOP AT mt_elements INTO lr_element.
    IF lr_element EQ ir_element.
      rv_exist = abap_true.
      RETURN.
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD length.
  DESCRIBE TABLE mt_elements LINES rv_length.
ENDMETHOD.


METHOD remove.
  rv_removed = abap_false.

  IF iv_index IS SUPPLIED AND iv_index IS NOT INITIAL.
    DELETE mt_elements INDEX iv_index.
    IF sy-subrc IS INITIAL.
      rv_removed = abap_true.
    ENDIF.
  ELSEIF ir_element IS BOUND.
    "TODO: remove element when IS_EQUAL method is implemented
    DELETE mt_elements WHERE table_line EQ ir_element.
    IF sy-subrc IS INITIAL.
      rv_removed = abap_true.
    ENDIF.
*    DATA: lr_element LIKE LINE OF mt_elements.
*
*    LOOP AT mt_elements INTO lr_element.
*      IF lr_element EQ ir_element.
*        rv_exist = abap_true.
*        RETURN.
*      ENDIF.
*    ENDLOOP.
  ENDIF.

ENDMETHOD.


  METHOD zif_json_element~as_data.


    DATA:
      lr_line  TYPE REF TO data,
      lr_descr TYPE REF TO cl_abap_typedescr,
      lr_ele   TYPE REF TO zif_json_element.

    FIELD-SYMBOLS:
      <ft_data>        TYPE ANY TABLE,
      <fs_line>        TYPE any,
      <table_sorted>   TYPE SORTED TABLE,
      <table_hashed>   TYPE HASHED TABLE,
      <table_standard> TYPE STANDARD TABLE.

    TRY.
        mr_tabledescr ?= cl_abap_tabledescr=>describe_by_data( cv_data ).

        ASSIGN cv_data TO <ft_data>.

        LOOP AT mt_elements INTO lr_ele.
          TRY .
              CREATE DATA lr_line LIKE LINE OF <ft_data>.
              ASSIGN lr_line->* TO <fs_line>.

              IF lr_ele->is_null( ) EQ abap_true.
                CONTINUE.
              ELSE.
                lr_ele->as_data( EXPORTING it_names_map = it_names_map
                        iv_map_ignore_rest = iv_map_ignore_rest
                        CHANGING cv_data = <fs_line> ).
              ENDIF.

              CASE mr_tabledescr->table_kind.
                WHEN cl_abap_tabledescr=>tablekind_sorted.
                  ASSIGN cv_data TO <table_sorted>.
                  INSERT <fs_line> INTO TABLE <table_sorted>.
                WHEN cl_abap_tabledescr=>tablekind_hashed.
                  ASSIGN cv_data TO <table_hashed>.
                  INSERT <fs_line> INTO TABLE <table_hashed>.
                WHEN OTHERS.
                  ASSIGN cv_data TO <table_standard>.
                  APPEND <fs_line> TO <table_standard>.
              ENDCASE.
            CATCH cx_root..
              "Not a Valid Table Line Type
              zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Table Line' ).
          ENDTRY.
        ENDLOOP.

      CATCH cx_root.
        "Not a Valid Table Type
        zcx_json_exception=>raise( iv_msg_number = '001' iv_msgv1 = 'Table' ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_json_element~as_string.
    rv_string = to_string( ).
  ENDMETHOD.


METHOD Zif_json_element~deep_copy.

  DATA: lr_result TYPE REF TO Zcl_json_array,
        lr_item TYPE REF TO Zif_json_element.

  CREATE OBJECT lr_result.

  lr_item = get_first( ).
  WHILE lr_item IS BOUND.
    lr_result->add( ir_element = lr_item->deep_copy( ) ).
    lr_item = get_next( ).
  ENDWHILE.

*  rv_element = me.
  rv_element = lr_result.

ENDMETHOD.


METHOD Zif_json_element~equals.

  DATA: lr_that TYPE REF TO Zcl_json_array.

  FIELD-SYMBOLS: <fr_element> LIKE LINE OF mt_elements,
                 <fr_ele_that> LIKE LINE OF mt_elements.

  rv_equal = abap_false.

  IF ir_element IS BOUND.
    CHECK ir_element->is_array( ) EQ abap_true. "if other object is an Array?

    lr_that ?= ir_element.

    CHECK lines( mt_elements ) EQ lines( lr_that->mt_elements ). "if both has same number of elements?

    LOOP AT mt_elements ASSIGNING <fr_element>. "deep compare
      READ TABLE lr_that->mt_elements ASSIGNING <fr_ele_that> INDEX sy-tabix.
      CHECK <fr_element>->equals( ir_element = <fr_ele_that> ) EQ abap_true.
    ENDLOOP.

  ELSEIF ir_data IS BOUND.
    RETURN. "not null
  ENDIF.

  rv_equal = abap_true.
ENDMETHOD.


method ZIF_JSON_ELEMENT~GET_ABAP_TYPE.
*CALL METHOD SUPER->ZIF_JSON_ELEMENT~GET_ABAP_TYPE
*  RECEIVING
*    RR_TYPE =
*    .
endmethod.


METHOD Zif_json_element~get_type.
  rv_type = Zif_json_element=>c_type_array.
ENDMETHOD.


METHOD Zif_json_element~is_array.
  iv_return = abap_true.
ENDMETHOD.


METHOD Zif_json_element~to_dref.

  FIELD-SYMBOLS: <fr_element> LIKE LINE OF mt_elements.

  DATA: lr_data TYPE REF TO data,
        lr_descr TYPE REF TO cl_abap_datadescr,
        lr_table_descr TYPE REF TO cl_abap_tabledescr.

  DATA: lr_dref TYPE REF TO data.

  FIELD-SYMBOLS: <ft_table> TYPE STANDARD TABLE,
                 <fr_value> TYPE any.

  lr_descr ?= cl_abap_datadescr=>describe_by_data( lr_data ).

  lr_table_descr ?= cl_abap_tabledescr=>create( p_line_type = lr_descr ).

  CREATE DATA rr_dref TYPE HANDLE lr_table_descr.

  ASSIGN rr_dref->* TO <ft_table>.

  LOOP AT mt_elements ASSIGNING <fr_element>.

    lr_dref = <fr_element>->to_dref( ).
    APPEND lr_dref TO <ft_table>.

  ENDLOOP.

ENDMETHOD.


METHOD Zif_json_element~to_string.
* [
*   333,
*   { "string" : "some text" },
*   null,
*   [ 33 , 44 , 488 ]
* ]

  FIELD-SYMBOLS: <fs_element> LIKE LINE OF mt_elements.
  DATA: lv_tmp_str TYPE string,
        lv_len TYPE i,
        lv_tabix TYPE i.

  DESCRIBE TABLE mt_elements LINES lv_len.

  rv_string = ''.

  IF mt_elements IS INITIAL AND mr_table IS BOUND.
    "inflate from data table ref
    rv_string = Zcl_json=>serialize( data = mr_table ).
  ELSE.
    "inflate from elements table
    IF ir_stream IS BOUND.
      ir_stream->write( `[` ).
      LOOP AT mt_elements ASSIGNING <fs_element>.
        lv_tabix = sy-tabix.

        <fs_element>->to_string( ir_stream = ir_stream ).

        IF lv_len NE lv_tabix.
          ir_stream->write( `,` ).
        ENDIF.
      ENDLOOP.
      ir_stream->write( `]` ).
    ELSE.

      LOOP AT mt_elements ASSIGNING <fs_element>.
        lv_tabix = sy-tabix.

        lv_tmp_str = <fs_element>->to_string( ir_stream = ir_stream ).

        IF lv_len NE lv_tabix.
          CONCATENATE rv_string lv_tmp_str `,` INTO rv_string.
        ELSE.
          CONCATENATE rv_string lv_tmp_str INTO rv_string.
        ENDIF.

      ENDLOOP.

      CONCATENATE `[` rv_string `]` INTO rv_string.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD Zif_json_iterator~get_by_index.

  IF iv_index LT 0 OR iv_index GE lines( mt_elements ).
*    mv_iterator_mark = iv_index.
    "Out of Bounds
    Zcx_json_exception=>raise( iv_msg_number = '002' iv_msgv1 = 'Array' ).
  ENDIF.
  mv_iterator_mark = iv_index + 1.
  READ TABLE mt_elements INTO r_result INDEX mv_iterator_mark.

ENDMETHOD.


METHOD Zif_json_iterator~get_current.
  READ TABLE mt_elements INTO r_result INDEX mv_iterator_mark.
ENDMETHOD.


METHOD Zif_json_iterator~get_first.
  mv_iterator_mark = 1.
  IF mv_iterator_mark GT lines( mt_elements ).
    SUBTRACT 1 FROM mv_iterator_mark.
    CLEAR: r_result.
*    Zcx_json_exception=>raise( iv_msg = 'Out of Bounds' ).
    RETURN.
  ENDIF.
  READ TABLE mt_elements INTO r_result INDEX mv_iterator_mark.

ENDMETHOD.


METHOD Zif_json_iterator~get_last.

  mv_iterator_mark = lines( mt_elements ).
  READ TABLE mt_elements INTO r_result INDEX mv_iterator_mark.
ENDMETHOD.


METHOD Zif_json_iterator~get_next.

  ADD 1 TO mv_iterator_mark .

  IF mv_iterator_mark GT lines( mt_elements ).
    SUBTRACT 1 FROM mv_iterator_mark.
    CLEAR: r_result.
*    Zcx_json_exception=>raise( iv_msg = 'Out of Bounds' ).
    RETURN.
  ENDIF.

  READ TABLE mt_elements INTO r_result INDEX mv_iterator_mark.

ENDMETHOD.


METHOD Zif_json_iterator~get_previous.
  SUBTRACT 1 FROM mv_iterator_mark .

  IF mv_iterator_mark LE 0.
    ADD 1 TO mv_iterator_mark.
    CLEAR: r_result.
*    Zcx_json_exception=>raise( iv_msg = 'Out of Bounds' ).
    RETURN.
  ENDIF.

  READ TABLE mt_elements INTO r_result INDEX mv_iterator_mark.

ENDMETHOD.


METHOD Zif_json_iterator~size.
  rv_size = lines( mt_elements ).
ENDMETHOD.
ENDCLASS.
