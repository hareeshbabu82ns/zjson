INTERFACE zif_json_iterator
  PUBLIC .


  METHODS get_first
    RETURNING
      VALUE(r_result) TYPE REF TO zif_json_element .
  METHODS get_next
    RETURNING
      VALUE(r_result) TYPE REF TO zif_json_element .
  METHODS get_previous
    RETURNING
      VALUE(r_result) TYPE REF TO zif_json_element .
  METHODS get_current
    RETURNING
      VALUE(r_result) TYPE REF TO zif_json_element .
  METHODS get_last
    RETURNING
      VALUE(r_result) TYPE REF TO zif_json_element .
  METHODS get_by_index
    IMPORTING
      !iv_index       TYPE int4
    RETURNING
      VALUE(r_result) TYPE REF TO zif_json_element .
  METHODS size
    RETURNING
      VALUE(rv_size) TYPE sytabix .
  METHODS get_current_index
    RETURNING
      VALUE(rv_result) TYPE sytabix .
ENDINTERFACE.
