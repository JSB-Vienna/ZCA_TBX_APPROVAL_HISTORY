"! <p class="shorttext synchronized" lang="en">Common object: Approval history write new entries</p>
FUNCTION z_ca_appr_hist_write.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_APPR_HIST) TYPE  ZCA_APPRHIST
*"  EXCEPTIONS
*"      INSERT_FAILED
*"----------------------------------------------------------------------
  INSERT zca_apprhist FROM is_appr_hist.
  IF sy-subrc NE 0.
    data(lv_key_for_output) = zcl_ca_wf_utils=>prepare_object_key_for_ouput( is_appr_hist-s_key ).
    "Inserting entry into table &1 with key &2&3 failed
    MESSAGE e010(zca_toolbox) WITH 'ZCA_APPRHIST' ##no_text
                                   lv_key_for_output(50)
                                   lv_key_for_output+50(50)
                                                RAISING insert_failed.
  ENDIF.
ENDFUNCTION.
