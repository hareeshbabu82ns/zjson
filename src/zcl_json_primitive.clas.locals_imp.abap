*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

DEFINE escape_json_inplace.
  REPLACE ALL OCCURRENCES OF `\` IN &1 WITH `\\`.
  REPLACE ALL OCCURRENCES OF `"` IN &1 WITH `\"`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN &1 WITH `\n`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN &1 WITH `\n`.
  "REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN &1 WITH `\t`.
END-OF-DEFINITION.

DEFINE escape_json.
  MOVE &1 TO &2.
  escape_json_inplace &2.
END-OF-DEFINITION.

DEFINE dump_type.

  CASE &2->type_kind.
    WHEN cl_abap_typedescr=>typekind_float OR cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1 OR
         cl_abap_typedescr=>typekind_int2 OR cl_abap_typedescr=>typekind_packed OR `8`. " TYPEKIND_INT8 -> '8' only from 7.40.
      IF &1 IS INITIAL.
        &3 = `0`.
      ELSE.
        MOVE &1 TO &3.
        IF &1 LT 0.
          SHIFT &3 RIGHT CIRCULAR.
        ELSE.
          CONDENSE &3.
        ENDIF.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_num.
      IF &1 IS INITIAL.
        &3 = `0`.
      ELSE.
        MOVE &1 TO &3.
        SHIFT &3 LEFT DELETING LEADING ` 0`.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_string OR cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_clike.
      IF &1 IS INITIAL.
        &3 = `""`.
      ELSE.
        escape_json &1 &3.
        CONCATENATE `"` &3 `"` INTO &3.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
      IF &1 IS INITIAL.
        &3 = `""`.
      ELSE.
        &3 = xstring_to_string( &1 ).
        escape_json_inplace &3.
        CONCATENATE `"` &3 `"` INTO &3.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_char.
      IF &2->output_length EQ 1 AND mc_boolean_types CS &2->absolute_name.
        IF &1 EQ abap_true.
          &3 = `true`.                                      "#EC NOTEXT
        ELSE.
          &3 = `false`.                                     "#EC NOTEXT
        ENDIF.
      ELSE.
        escape_json &1 &3.
        CONCATENATE `"` &3 `"` INTO &3.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_date.
      CONCATENATE `"` &1(4) `-` &1+4(2) `-` &1+6(2) `"` INTO &3.
    WHEN cl_abap_typedescr=>typekind_time.
      CONCATENATE `"` &1(2) `:` &1+2(2) `:` &1+4(2) `"` INTO &3.
    WHEN OTHERS.
      IF &1 IS INITIAL.
        &3 = `null`.                                        "#EC NOTEXT
      ELSE.
        MOVE &1 TO &3.
      ENDIF.
  ENDCASE.

END-OF-DEFINITION.
