interface ZIF_JSON_ITERATOR
  public .


  methods GET_FIRST
    returning
      value(R_RESULT) type ref to ZIF_JSON_ELEMENT .
  methods GET_NEXT
    returning
      value(R_RESULT) type ref to ZIF_JSON_ELEMENT .
  methods GET_PREVIOUS
    returning
      value(R_RESULT) type ref to ZIF_JSON_ELEMENT .
  methods GET_CURRENT
    returning
      value(R_RESULT) type ref to ZIF_JSON_ELEMENT .
  methods GET_LAST
    returning
      value(R_RESULT) type ref to ZIF_JSON_ELEMENT .
  methods GET_BY_INDEX
    importing
      !IV_INDEX type INT4
    returning
      value(R_RESULT) type ref to ZIF_JSON_ELEMENT .
  methods SIZE
    returning
      value(RV_SIZE) type SYTABIX .
  methods GET_CURRENT_INDEX
    returning
      value(RV_RESULT) type SYTABIX .
endinterface.
