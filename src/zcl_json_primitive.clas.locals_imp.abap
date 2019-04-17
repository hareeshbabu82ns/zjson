*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

DEFINE escape_json_inplace.
  replace all occurrences of `\` in &1 with `\\`.
  replace all occurrences of `"` in &1 with `\"`.
END-OF-DEFINITION.

DEFINE escape_json.
  move &1 to &2.
  escape_json_inplace &2.
END-OF-DEFINITION.

DEFINE dump_type.

  case &2->type_kind.
    when cl_abap_typedescr=>typekind_float or cl_abap_typedescr=>typekind_int or cl_abap_typedescr=>typekind_int1 or
         cl_abap_typedescr=>typekind_int2 or cl_abap_typedescr=>typekind_packed or `8`. " TYPEKIND_INT8 -> '8' only from 7.40.
      if &1 is initial.
        &3 = `0`.
      else.
        move &1 to &3.
        if &1 lt 0.
          shift &3 right circular.
        else.
          condense &3.
        endif.
      endif.
    when cl_abap_typedescr=>typekind_num.
      if &1 is initial.
        &3 = `0`.
      else.
        move &1 to &3.
        shift &3 left deleting leading ` 0`.
      endif.
    when cl_abap_typedescr=>typekind_string or cl_abap_typedescr=>typekind_csequence or cl_abap_typedescr=>typekind_clike.
      if &1 is initial.
        &3 = `""`.
      else.
        escape_json &1 &3.
        concatenate `"` &3 `"` into &3.
      endif.
    when cl_abap_typedescr=>typekind_xstring or cl_abap_typedescr=>typekind_hex.
      if &1 is initial.
        &3 = `""`.
      else.
        &3 = xstring_to_string( &1 ).
        escape_json_inplace &3.
        concatenate `"` &3 `"` into &3.
      endif.
    when cl_abap_typedescr=>typekind_char.
      if &2->output_length eq 1 and mc_boolean_types cs &2->absolute_name.
        if &1 eq abap_true.
          &3 = `true`.                                      "#EC NOTEXT
        else.
          &3 = `false`.                                     "#EC NOTEXT
        endif.
      else.
        escape_json &1 &3.
        concatenate `"` &3 `"` into &3.
      endif.
    when cl_abap_typedescr=>typekind_date.
      concatenate `"` &1(4) `-` &1+4(2) `-` &1+6(2) `"` into &3.
    when cl_abap_typedescr=>typekind_time.
      concatenate `"` &1(2) `:` &1+2(2) `:` &1+4(2) `"` into &3.
    when others.
      if &1 is initial.
        &3 = `null`.                                        "#EC NOTEXT
      else.
        move &1 to &3.
      endif.
  endcase.

END-OF-DEFINITION.
