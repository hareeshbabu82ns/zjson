class ZCX_JSON_EXCEPTION definition
  public
  inheriting from CX_NO_CHECK
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  data ERRORS type BAPIRET2_T read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !ERRORS type BAPIRET2_T optional .
  class-methods RAISE
    importing
      !IV_MSG type STRING optional
      !PREVIOUS like PREVIOUS optional
      !IV_TYPE type BAPI_MTYPE default 'E'
      !IV_MSG_ID type SY-MSGID default 'ZJSON'
      !IV_MSG_NUMBER type SY-MSGNO default 000
      !IV_MSGV1 type ANY optional
      !IV_MSGV2 type ANY optional
      !IV_MSGV3 type ANY optional
      !IV_MSGV4 type ANY optional
      !IT_ERRORS type BAPIRET2_T optional
      !IS_ERROR type BAPIRET2 optional .
  class-methods BUILD_MESSAGES
    importing
      !IV_TYPE type BAPIRETURN-TYPE default 'E'
      !IV_MSG_ID type SY-MSGID default 'ZJSON'
      !IV_MSG_NUMBER type SY-MSGNO default 000
      !IV_MSGV1 type ANY optional
      !IV_MSGV2 type ANY optional
      !IV_MSGV3 type ANY optional
      !IV_MSGV4 type ANY optional
    changing
      !CT_RETURN type BAPIRET2_T .
  methods GET_ERRORS
    returning
      value(RT_ERROR) type ZT_STRING .

  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
  methods IF_MESSAGE~GET_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCX_JSON_EXCEPTION IMPLEMENTATION.


METHOD build_messages.
  DATA: ls_return TYPE bapiret2,
      lv_msgv1 TYPE sy-msgv1,
      lv_msgv2 TYPE sy-msgv2,
      lv_msgv3 TYPE sy-msgv3,
      lv_msgv4 TYPE sy-msgv4.

  lv_msgv1 = iv_msgv1.
  lv_msgv2 = iv_msgv2.
  lv_msgv3 = iv_msgv3.
  lv_msgv4 = iv_msgv4.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
    EXPORTING
      type   = iv_type
      cl     = iv_msg_id
      number = iv_msg_number
      par1   = lv_msgv1
      par2   = lv_msgv2
      par3   = lv_msgv3
      par4   = lv_msgv4
    IMPORTING
      return = ls_return.
  APPEND ls_return TO ct_return.
ENDMETHOD.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->ERRORS = ERRORS .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


METHOD get_errors.

  DATA: ls_err TYPE bapiret2,
        lv_str TYPE string.

  LOOP AT errors INTO ls_err.

    MESSAGE ID ls_err-id TYPE ls_err-type NUMBER ls_err-number
             WITH ls_err-message_v1 ls_err-message_v2
                  ls_err-message_v3 ls_err-message_v4 INTO lv_str.

    IF lv_str IS INITIAL.
      "check if message has some text
      lv_str = ls_err-message.
    ENDIF.

    IF lv_str IS INITIAL.
      "if still message is empty, do not add into final table
      CONTINUE.
    ENDIF.

    APPEND lv_str TO rt_error.
    CLEAR lv_str.
  ENDLOOP.

ENDMETHOD.


METHOD if_message~get_longtext.

  DATA: ls_err TYPE bapiret2,
        lv_str TYPE string.

  IF errors IS INITIAL.
    CALL METHOD super->if_message~get_longtext
      EXPORTING
        preserve_newlines = preserve_newlines
      RECEIVING
        result            = result.
  ELSE.
    LOOP AT errors INTO ls_err.
      MESSAGE ID ls_err-id TYPE ls_err-type NUMBER ls_err-number
             WITH ls_err-message_v1 ls_err-message_v2
                  ls_err-message_v3 ls_err-message_v4 INTO lv_str.
      result = result && cl_abap_char_utilities=>cr_lf && lv_str.
    ENDLOOP.
  ENDIF.

ENDMETHOD.


METHOD if_message~get_text.
  DATA: ls_err TYPE bapiret2.

  IF errors IS INITIAL.
    CALL METHOD super->if_message~get_text
      RECEIVING
        result = result.
  ELSE.
    READ TABLE errors INTO ls_err INDEX 1.
    MESSAGE ID ls_err-id TYPE ls_err-type NUMBER ls_err-number
           WITH ls_err-message_v1 ls_err-message_v2
                ls_err-message_v3 ls_err-message_v4 INTO result.
  ENDIF.
ENDMETHOD.


METHOD raise.

  DATA: ls_err TYPE bapiret2,
        lt_err TYPE bapiret2_t,
        lv_msgv1 TYPE sy-msgv1,
        lv_msgv2 TYPE sy-msgv2,
        lv_msgv3 TYPE sy-msgv3,
        lv_msgv4 TYPE sy-msgv4.

  IF it_errors IS NOT INITIAL.
    APPEND LINES OF it_errors TO lt_err.

  ELSEIF is_error IS NOT INITIAL.
    APPEND is_error TO lt_err.

  ELSE.
    lv_msgv1 = iv_msgv1.
    lv_msgv2 = iv_msgv2.
    lv_msgv3 = iv_msgv3.
    lv_msgv4 = iv_msgv4.

    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = iv_type
        cl     = iv_msg_id
        number = iv_msg_number
        par1   = lv_msgv1
        par2   = lv_msgv2
        par3   = lv_msgv3
        par4   = lv_msgv4
      IMPORTING
        return = ls_err.

    ls_err-message = iv_msg.

    APPEND ls_err TO lt_err.

  ENDIF.

  IF previous IS BOUND.
    DATA: lr_cx TYPE REF TO cx_root,
          lr_cx_json TYPE REF TO zcx_json_exception.
    lr_cx = previous.
    WHILE lr_cx IS BOUND.
      TRY .
          "check if previous is CX_JSON type
          lr_cx_json ?= lr_cx.
          APPEND LINES OF lr_cx_json->errors TO lt_err.
          lr_cx = lr_cx_json->previous.
          CONTINUE.
        CATCH cx_root.
      ENDTRY.
      CLEAR ls_err.
      CALL FUNCTION 'BALW_BAPIRETURN_GET2'
        EXPORTING
          type   = iv_type
          cl     = iv_msg_id
          number = '000'
        IMPORTING
          return = ls_err.
      ls_err-message = lr_cx->get_text( ).
      APPEND ls_err TO lt_err.
      lr_cx = lr_cx->previous.
    ENDWHILE.
  ENDIF.

  RAISE EXCEPTION TYPE zcx_json_exception
    EXPORTING
      previous = previous
      errors   = lt_err.

ENDMETHOD.
ENDCLASS.
