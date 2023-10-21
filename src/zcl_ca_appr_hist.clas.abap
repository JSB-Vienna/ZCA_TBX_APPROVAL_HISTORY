"! <p class="shorttext synchronized" lang="en">CA-TBX: Handler for approval history</p>
CLASS zcl_ca_appr_hist DEFINITION PUBLIC
                                  CREATE PUBLIC.

* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n t e r f a c e s
    INTERFACES:
      if_xo_const_message.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Constants for approval history result</p>
      mo_result_values   TYPE REF TO zcl_ca_c_approval_result READ-ONLY,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Approval history data (in descending order)</p>
      mt_appr_hist       TYPE zca_tt_appr_hist READ-ONLY,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Business object/class key - BOR Compatible</p>
      ms_bo_key          TYPE sibflporb READ-ONLY,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Current cycle number</p>
      mv_current_cycle   TYPE zca_d_approval_cycle READ-ONLY,
      "! <p class="shorttext synchronized" lang="en">Current approval level</p>
      mv_curr_appr_level TYPE zca_d_approval_level READ-ONLY.


*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Add approver (= receiver) for next approval level</p>
      "!
      "! @parameter iv_approver  | <p class="shorttext synchronized" lang="en">User Id of approving (= receiving) person</p>
      "! @parameter iv_for_level | <p class="shorttext synchronized" lang="en">Approval level (initial value increases automatically by 1)</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">Key to identify receiver for approval</p>
      "! @raising   zcx_ca_param | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      add_approver
        IMPORTING
          iv_approver   TYPE xubname
          iv_for_level  TYPE zca_d_approval_level OPTIONAL
        RETURNING
          VALUE(result) TYPE zca_s_approval_sequence_key
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,


      "! <p class="shorttext synchronized" lang="en">Display approval history</p>
      "!
      "! @parameter iv_obj_name            | <p class="shorttext synchronized" lang="en">Object name as addition for the title</p>
      "! @parameter is_popup_corners       | <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      "! @parameter iv_for_last_cycle_only | <p class="shorttext synchronized" lang="en">X = Return only entries of the last cycle</p>
      "! @parameter io_parent              | <p class="shorttext synchronized" lang="en">GUI container for display anywhere, else display in a popup</p>
      "! @parameter iv_cnt_name            | <p class="shorttext synchronized" lang="en">Name of the ALV control/container</p>
      "! @raising   zcx_ca_appr_hist       | <p class="shorttext synchronized" lang="en">Common exception: Error while handling approval history</p>
      display
        IMPORTING
          iv_obj_name            TYPE text30 OPTIONAL
          is_popup_corners       TYPE zca_s_scr_fw_popup_corners OPTIONAL
          iv_for_last_cycle_only TYPE abap_boolean DEFAULT abap_true
          io_parent              TYPE REF TO cl_gui_container OPTIONAL
          iv_cnt_name            TYPE csequence OPTIONAL
        RAISING
          zcx_ca_appr_hist,

      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter is_bo_key        | <p class="shorttext synchronized" lang="en">Business object/class key - BOR Compatible</p>
      "! @raising   zcx_ca_appr_hist | <p class="shorttext synchronized" lang="en">Common exception: Error while handling approval history</p>
      constructor
        IMPORTING
          is_bo_key TYPE sibflporb
        RAISING
          zcx_ca_appr_hist,

      "! <p class="shorttext synchronized" lang="en">Check whether open approvals exist</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">X = Open approvals exist for current level</p>
      has_open_approvals
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Replacing approvers in open approval history entries</p>
      "!
      "! <p>This can be required if e. g. the WF admin repeats the agent determination due to organizational
      "! changes.</p>
      "! <p>Since there can be several approvers per level the new approvers have to be passed as a table. The
      "! method changes only <strong>open approvals</strong>.</p>
      "! <p>If the approval history is used together with a workflow then the <strong>caller is responsible</strong>
      "! for the task handling (creating new / completing open tasks). Date and time will be reset to the current
      "! date and time.</p>
      "!
      "! @parameter it_new_approvers | <p class="shorttext synchronized" lang="en">New user Id to be set</p>
      "! @raising   zcx_ca_param     | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc     | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      replace_approvers
        IMPORTING
          it_new_approvers TYPE rsec_t_users
        RAISING
          zcx_ca_param
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Get approval history data to this key</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Approval history data</p>
      get
        RETURNING
          VALUE(result) TYPE zca_tt_appr_hist,

      "! <p class="shorttext synchronized" lang="en">Get prepared data for output in descending order</p>
      "!
      "! @parameter iv_for_last_cycle_only | <p class="shorttext synchronized" lang="en">X = Return only entries of the last cycle</p>
      "! @parameter result                 | <p class="shorttext synchronized" lang="en">Prepared approval history data in descending order</p>
      get_approval_list_for_display
        IMPORTING
          iv_for_last_cycle_only TYPE abap_boolean DEFAULT abap_true
        RETURNING
          VALUE(result)          TYPE zca_tt_approval_list,

      "! <p class="shorttext synchronized" lang="en">Get current approval cycle and level</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Current approval cycle and level</p>
      get_current_cycle_n_level
        RETURNING
          VALUE(result) TYPE zca_s_approval_sequence_key,

      "! <p class="shorttext synchronized" lang="en">Get approval history data to this key</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Last approval history entry</p>
      get_last_entry
        RETURNING
          VALUE(result) TYPE zca_apprhist,

      "! <p class="shorttext synchronized" lang="en">Increase cycle counter due to a rejection</p>
      increase_cycle_due_2_rejection,

      "! <p class="shorttext synchronized" lang="en">Check if last approval cycle is rejected</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">X = Last approval cycle is rejected</p>
      is_last_cycle_rejected
        RETURNING
          VALUE(result) TYPE abap_boolean,

      "! <p class="shorttext synchronized" lang="en">Update approval history entry with approval result</p>
      "!
      "! @parameter is_appr_seq_key    | <p class="shorttext synchronized" lang="en">Approval sequence key for approval result assignment</p>
      "! @parameter iv_actual_approver | <p class="shorttext synchronized" lang="en">User Id of approving / rejecting person</p>
      "! @parameter iv_approval_result | <p class="shorttext synchronized" lang="en">Approval result (use mo_result_values->*)</p>
      "! @parameter iv_reason          | <p class="shorttext synchronized" lang="en">Reason / comment to approval decision</p>
      "! @raising   zcx_ca_param       | <p class="shorttext synchronized" lang="en">Common exception: Parameter error (INHERIT from this excep!)</p>
      "! @raising   zcx_ca_dbacc       | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      write_approval_result
        IMPORTING
          is_appr_seq_key    TYPE zca_s_approval_sequence_key
          iv_actual_approver TYPE xubname
          iv_approval_result TYPE swf_appres
          iv_reason          TYPE zca_d_reason_decision OPTIONAL
        RAISING
          zcx_ca_param
          zcx_ca_dbacc.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   a l i a s e s
    ALIASES:
*     Message types
      c_msgty_e            FOR  if_xo_const_message~error,
      c_msgty_i            FOR  if_xo_const_message~info,
      c_msgty_s            FOR  if_xo_const_message~success,
      c_msgty_w            FOR  if_xo_const_message~warning.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Last sequential number</p>
      mv_last_seqno    TYPE tprlfdnr.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Get approval history from data base</p>
      get_from_db,

      "! <p class="shorttext synchronized" lang="en">Get approval history data to requested level</p>
      "!
      "! @parameter iv_approval_level | <p class="shorttext synchronized" lang="en">Approval level</p>
      "! @parameter result            | <p class="shorttext synchronized" lang="en">Last approval history entry to level</p>
      get_last_entry_to_level
        IMPORTING
          iv_approval_level TYPE zca_d_approval_level
        RETURNING
          VALUE(result)     TYPE zca_apprhist,

      "! <p class="shorttext synchronized" lang="en">Insert new entry into database table</p>
      "!
      "! @parameter is_new_entry | <p class="shorttext synchronized" lang="en">New approval history entry</p>
      "! @raising   zcx_ca_dbacc | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      insert_new_entry
        IMPORTING
          is_new_entry TYPE zca_apprhist
        RAISING
          zcx_ca_dbacc,

      "! <p class="shorttext synchronized" lang="en">Update approval history entry in database table</p>
      "!
      "! @parameter is_appr_entry_backup | <p class="shorttext synchronized" lang="en">Copy of the last state of the approval entry</p>
      "! @parameter cr_appr_entry        | <p class="shorttext synchronized" lang="en">Approval history entry with changed result values</p>
      "! @raising   zcx_ca_dbacc         | <p class="shorttext synchronized" lang="en">Common exception: Database access</p>
      update_result
        IMPORTING
          is_appr_entry_backup TYPE zca_apprhist
        CHANGING
          cr_appr_entry        TYPE REF TO zca_apprhist
        RAISING
          zcx_ca_dbacc.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_ca_appr_hist IMPLEMENTATION.

  METHOD add_approver.
    "-----------------------------------------------------------------*
    "   Add approver (= receiver) for next approval level
    "-----------------------------------------------------------------*
    GET TIME.
    "Prepares key exception sequence no. in case no entry is found
    DATA(ls_new_entry) = get_last_entry_to_level( iv_for_level ).

    IF ls_new_entry-approval_level IS INITIAL.
      ls_new_entry-approval_level += 1.
      ls_new_entry-approval_level  = |{ ls_new_entry-approval_level ALPHA = OUT }|.
    ENDIF.

    ls_new_entry-seqno              += 1.
    ls_new_entry-receiving_user_id   = iv_approver.

    ls_new_entry-receiving_user_name = zcl_ca_wf_user=>get_instance( iv_key = iv_approver )->ms_address-fullname.

    ls_new_entry-date_received       = sy-datlo.
    ls_new_entry-time_received       = sy-uzeit.

    insert_new_entry( ls_new_entry ).

    "Keep successful inserted data in instance variables
    INSERT ls_new_entry INTO  mt_appr_hist
                        INDEX 1.

    result = ls_new_entry-s_seq_key.
  ENDMETHOD.                    "add_approver


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    "Key value is set?
    IF is_bo_key-instid IS INITIAL.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_appr_hist
        EXPORTING
          textid   = zcx_ca_appr_hist=>param_invalid
          mv_msgty = c_msgty_e
          mv_msgv1 = 'IS_BO_KEY-INSTID'
          mv_msgv2 = 'SPACE' ##no_text.
    ENDIF.

    CASE is_bo_key-catid.
      WHEN swfco_objtype_bor.
        "Business object
        "Check business object
        SELECT SINGLE * INTO  @DATA(ls_tojtb)
                        FROM  tojtb
                        WHERE name   EQ @is_bo_key-typeid
                          AND active EQ @abap_true.
        IF sy-subrc NE 0.
          "SAP business object &1 does not exist or is not activated
          RAISE EXCEPTION TYPE zcx_ca_appr_hist
            EXPORTING
              textid   = zcx_ca_appr_hist=>bo_not_exist
              mv_msgty = c_msgty_e
              mv_msgv1 = CONV #( is_bo_key-typeid ).
        ENDIF.

      WHEN swfco_objtype_cl.
        "Workflow class
        TRY.
            DATA(lo_obj_desc) = CAST cl_abap_objectdescr(
                                           NEW zcl_ca_ddic( iv_name = is_bo_key-typeid )->mo_type_desc ).

          CATCH zcx_ca_param INTO DATA(lx_error).
            "SAP business object &1 does not exist or is not activated
            RAISE EXCEPTION TYPE zcx_ca_appr_hist
              EXPORTING
                textid   = zcx_ca_appr_hist=>bo_not_exist
                mv_msgty = c_msgty_e
                mv_msgv1 = CONV #( is_bo_key-typeid ).
        ENDTRY.

      WHEN OTHERS.
        "Parameter '&1' has invalid value '&2'
        RAISE EXCEPTION TYPE zcx_ca_appr_hist
          EXPORTING
            textid   = zcx_ca_appr_hist=>param_invalid
            mv_msgty = c_msgty_e
            mv_msgv1 = 'IS_BO_KEY-CATID' ##no_text
            mv_msgv2 = CONV #( is_bo_key-catid ).
    ENDCASE.

    ms_bo_key = is_bo_key.
    mo_result_values = zcl_ca_c_approval_result=>get_instance( ).

    get( ).
  ENDMETHOD.                    "constructor


  METHOD display.
    "-----------------------------------------------------------------*
    "   Display approval history as popup
    "-----------------------------------------------------------------*
    TRY.
        IF mt_appr_hist IS INITIAL.
          "Currently are no entries available to display
          RAISE EXCEPTION TYPE zcx_ca_appr_hist
            EXPORTING
              textid   = zcx_ca_appr_hist=>curr_no_entries
              mv_msgty = c_msgty_s.
        ENDIF.

        DATA(ls_popup_corners) = is_popup_corners.
        IF ls_popup_corners IS INITIAL.
          ls_popup_corners = VALUE #( starting_at_x = 10
                                      ending_at_x   = 180
                                      starting_at_y = 8
                                      ending_at_y   = 16 ).
        ENDIF.

*        IF io_parent IS NOT BOUND.
*          DATA(lo_popup) = NEW zcl_ca_appr_hist_popup_details( io_appr_hist     = me
*                                                               iv_obj_name      = iv_obj_name
*                                                               is_popup_corners = ls_popup_corners ).
*          lo_popup->display( ).
*
*        ELSE.
        NEW zcl_ca_appr_hist_alv( io_appr_hist     = me
                                  iv_obj_name      = iv_obj_name
                                  io_parent        = io_parent
                                  is_popup_corners = ls_popup_corners
                                  iv_cnt_name      = iv_cnt_name )->process( ).
*        ENDIF.

      CATCH zcx_ca_appr_hist INTO DATA(lx_error).
        MESSAGE lx_error TYPE c_msgty_s DISPLAY LIKE c_msgty_w.
    ENDTRY.
  ENDMETHOD.                    "display


  METHOD has_open_approvals.
    "-----------------------------------------------------------------*
    "   Check whether open approvals exist
    "-----------------------------------------------------------------*
    result = abap_false.
    DATA(ls_appr_seq_key) = get_current_cycle_n_level( ).
    IF line_exists( mt_appr_hist[ approval_cycle  = ls_appr_seq_key-approval_cycle
                                  approval_level  = ls_appr_seq_key-approval_level
                                  approval_result = mo_result_values->approval_result-open ] ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.                    "has_open_approvals


  METHOD get.
    "-----------------------------------------------------------------*
    "   Get approval history data and prepare
    "-----------------------------------------------------------------*
    get_from_db( ).

    result = mt_appr_hist.
  ENDMETHOD.                    "get


  METHOD get_approval_list_for_display.
    "-----------------------------------------------------------------*
    "   Get prepared data for output in descending order
    "-----------------------------------------------------------------*
    result = CORRESPONDING #( mt_appr_hist ).

    IF iv_for_last_cycle_only EQ abap_true.
      DELETE result WHERE approval_cycle NE mv_current_cycle.
    ENDIF.

    "Table is in descending sorted order of the receiving date/time. For output sort by approval date/time.
    SORT result BY date_received DESCENDING
                   time_received DESCENDING
                   approval_date DESCENDING
                   approval_time DESCENDING.

    LOOP AT result REFERENCE INTO DATA(lr_approval).
      lr_approval->text_result = mo_result_values->get_text_for_result( lr_approval->approval_result ).
      lr_approval->icon_result = mo_result_values->get_icon_for_result( lr_approval->approval_result ).
    ENDLOOP.
  ENDMETHOD.                    "get_approval_list_for_display


  METHOD get_current_cycle_n_level.
    "-----------------------------------------------------------------*
    "   Get current cycle and approval level
    "-----------------------------------------------------------------*
    result = get_last_entry( )-s_seq_key.
    IF result-approval_cycle NE mv_current_cycle.
      CLEAR result.
      result-approval_cycle = mv_current_cycle.
    ENDIF.
  ENDMETHOD.                    "get_current_cycle_n_level


  METHOD get_from_db.
    "-----------------------------------------------------------------*
    "   Get approval history data from database
    "-----------------------------------------------------------------*
    SELECT * INTO  TABLE @mt_appr_hist
             FROM  zca_apprhist
             WHERE instid EQ @ms_bo_key-instid
               AND typeid EQ @ms_bo_key-typeid
               AND catid  EQ @ms_bo_key-catid
                             ORDER BY date_received  DESCENDING,     "Date + time include the right sorting
                                      time_received  DESCENDING,     "of cycle and level, except the SEQNO
                                      seqno          DESCENDING.     "that can different within a second.

    DATA(lr_appr_hist) = REF #( mt_appr_hist[ 1 ] OPTIONAL ).
    IF lr_appr_hist IS NOT BOUND.
      mv_current_cycle = '01'.

    ELSE.
      mv_current_cycle = lr_appr_hist->approval_cycle.
    ENDIF.
  ENDMETHOD.                    "get_from_db


  METHOD get_last_entry.
    "-----------------------------------------------------------------*
    "   Get last / most recent approval history entry
    "-----------------------------------------------------------------*
    result = VALUE #( mt_appr_hist[ 1 ] OPTIONAL ).
  ENDMETHOD.                    "get_last_entry


  METHOD get_last_entry_to_level.
    "-----------------------------------------------------------------*
    "   Get approval history data to requested level
    "-----------------------------------------------------------------*
    TRY.
        "Table is sorted descending by level and sequence no
        result = mt_appr_hist[ approval_cycle = mv_current_cycle
                               approval_level = iv_approval_level ].

      CATCH cx_sy_itab_line_not_found.
        "Still no history available -> is first approval
        result-s_key          = ms_bo_key.
        result-approval_cycle = mv_current_cycle.
        result-approval_level = iv_approval_level.
    ENDTRY.
  ENDMETHOD.                    "get_last_entry_to_level


  METHOD increase_cycle_due_2_rejection.
    "-----------------------------------------------------------------*
    "   Increase cycle counter due to a rejection
    "-----------------------------------------------------------------*
    IF is_last_cycle_rejected( ).
      mv_current_cycle += 1.
    ENDIF.
  ENDMETHOD.                    "increase_cycle_due_2_rejection


  METHOD is_last_cycle_rejected.
    "-----------------------------------------------------------------*
    "   Check if last approval cycle is rejected
    "-----------------------------------------------------------------*
    result = abap_false.
    IF line_exists( mt_appr_hist[ approval_cycle  = mv_current_cycle
                                  approval_result = mo_result_values->approval_result-rejected ] ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.                    "is_last_cycle_rejected


  METHOD insert_new_entry.
    "-----------------------------------------------------------------*
    "   Insert new entry into database table
    "-----------------------------------------------------------------*
    INSERT zca_apprhist FROM is_new_entry.
    IF sy-subrc NE 0.
      DATA(lv_key_for_output) = zcl_ca_wf_utils=>prepare_object_key_for_ouput( is_new_entry-s_key ).
      lv_key_for_output =
             |{ is_new_entry-catid } { is_new_entry-typeid } { lv_key_for_output } | &
             |{ is_new_entry-approval_cycle ALPHA = OUT } { is_new_entry-approval_level ALPHA = OUT } | &
             |{ is_new_entry-seqno ALPHA = OUT }|.
      "Inserting entry into table &1 with key &2&3 failed
      RAISE EXCEPTION TYPE zcx_ca_dbacc
        EXPORTING
          textid   = zcx_ca_dbacc=>insert_failed
          mv_msgty = c_msgty_e
          mv_msgv1 = 'ZCA_APPRHIST'
          mv_msgv2 = lv_key_for_output(50)
          mv_msgv3 = lv_key_for_output+50(50) ##no_text.
    ENDIF.
  ENDMETHOD.                    "insert_new_entry


  METHOD replace_approvers.
    "-----------------------------------------------------------------*
    "   Correction of an open approval history entry -> see method description
    "-----------------------------------------------------------------*
    DATA(ls_appr_seq_key) = get_current_cycle_n_level( ).

    "Determine open entries of the level independent of the result
    DATA(lt_appr_hist_of_level) =
           VALUE zca_tt_appr_hist( FOR ls_appr_hist IN mt_appr_hist
                                            WHERE ( approval_cycle EQ ls_appr_seq_key-approval_cycle AND
                                                    approval_level EQ ls_appr_seq_key-approval_level )
                                                                            ( ls_appr_hist ) ).

    "Delete entries that have already one of the new users as receiver. If at least one occurred there is
    "no need to change something.
    DATA(lt_new_approvers) = it_new_approvers.
    LOOP AT lt_new_approvers INTO DATA(lv_new_approver).
      DELETE lt_appr_hist_of_level WHERE receiving_user_id EQ lv_new_approver.
      IF sy-subrc EQ 0.
        DELETE lt_new_approvers.
      ENDIF.
    ENDLOOP.

    "Delete remaining entries with a result - must not be changed
    DELETE lt_appr_hist_of_level WHERE approval_result NE mo_result_values->approval_result-open.

    GET TIME.
    DATA(lv_idx_new_approver) = 1.
    LOOP AT lt_appr_hist_of_level REFERENCE INTO DATA(lr_appr_entry).
      TRY.
          DATA(ls_appr_entry_backup) = lr_appr_entry->*.

          "Set new approver, date and time and actualize the full name
          lr_appr_entry->receiving_user_id = lt_new_approvers[ lv_idx_new_approver ].
          lv_idx_new_approver += 1.

          lr_appr_entry->date_received       = sy-datlo.
          lr_appr_entry->time_received       = sy-uzeit.
          lr_appr_entry->receiving_user_name =
                  zcl_ca_wf_user=>get_instance( iv_key = lr_appr_entry->receiving_user_id )->ms_address-fullname.

          update_result(
                   EXPORTING
                      is_appr_entry_backup = ls_appr_entry_backup
                   CHANGING
                     cr_appr_entry         = lr_appr_entry ).

        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.                    "replace_approvers


  METHOD update_result.
    "-----------------------------------------------------------------*
    "   Update approval history entry in database table
    "-----------------------------------------------------------------*
    UPDATE zca_apprhist FROM cr_appr_entry->*.
    IF sy-subrc NE 0.
      cr_appr_entry->* = is_appr_entry_backup.        "Roll back changes in internal table

      DATA(lv_key_for_output) = zcl_ca_wf_utils=>prepare_object_key_for_ouput( cr_appr_entry->s_key ).
      lv_key_for_output =
             |{ cr_appr_entry->catid } { cr_appr_entry->typeid } { lv_key_for_output } | &
             |{ cr_appr_entry->approval_cycle ALPHA = OUT } { cr_appr_entry->approval_level ALPHA = OUT } | &
             |{ cr_appr_entry->seqno ALPHA = OUT }|.
      "Updating entry in table &1 with key &2&3 failed
      RAISE EXCEPTION TYPE zcx_ca_dbacc
        EXPORTING
          textid   = zcx_ca_dbacc=>update_failed
          mv_msgty = c_msgty_e
          mv_msgv1 = 'ZCA_APPRHIST'
          mv_msgv2 = lv_key_for_output(50)
          mv_msgv3 = lv_key_for_output+50(50) ##no_text.
    ENDIF.
  ENDMETHOD.                    "update_result


  METHOD write_approval_result.
    "-----------------------------------------------------------------*
    "   Update approval history entry with approval result
    "-----------------------------------------------------------------*
    TRY.
        DATA(lr_appr_entry)        = REF #( mt_appr_hist[ s_seq_key = is_appr_seq_key ] ).
        DATA(ls_appr_entry_backup) = lr_appr_entry->*.

      CATCH cx_sy_itab_line_not_found.
        "Approval entry to level &1 (cycle &2) / sequence no. &3 not found
        RAISE EXCEPTION TYPE zcx_ca_appr_hist
          EXPORTING
            textid   = zcx_ca_appr_hist=>approval_entry_not_found
            mv_msgty = c_msgty_e
            mv_msgv1 = CONV #( |{ is_appr_seq_key-approval_level ALPHA = OUT }| )
            mv_msgv2 = CONV #( |{ is_appr_seq_key-approval_cycle ALPHA = OUT }| )
            mv_msgv3 = CONV #( |{ is_appr_seq_key-seqno ALPHA = OUT }| ).
    ENDTRY.

    GET TIME.
    lr_appr_entry->approval_result     = iv_approval_result.
    lr_appr_entry->reason              = iv_reason.
    lr_appr_entry->approval_date       = sy-datlo.
    lr_appr_entry->approval_time       = sy-uzeit.
    lr_appr_entry->approving_user_id   = iv_actual_approver.
    lr_appr_entry->approving_user_name =
                       zcl_ca_wf_user=>get_instance( iv_key = iv_actual_approver )->ms_address-fullname.

    update_result(
             EXPORTING
                is_appr_entry_backup = ls_appr_entry_backup
             CHANGING
               cr_appr_entry         = lr_appr_entry ).
  ENDMETHOD.                    "write_approval_result

ENDCLASS.
