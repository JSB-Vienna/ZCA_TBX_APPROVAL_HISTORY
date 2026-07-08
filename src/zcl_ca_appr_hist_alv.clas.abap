"! <p class="shorttext synchronized" lang="en">CA-TBX: ALV list with approval history</p>
CLASS zcl_ca_appr_hist_alv DEFINITION PUBLIC
                                      INHERITING FROM zcl_ca_salv_wrapper
                                      CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter io_appr_hist           | <p class="shorttext synchronized" lang="en">Approval history handler</p>
      "! @parameter iv_for_last_cycle_only | <p class="shorttext synchronized" lang="en">X = Return only entries of the last cycle</p>
      "! @parameter io_parent              | <p class="shorttext synchronized" lang="en">GUI Container to display</p>
      "! @parameter iv_obj_name            | <p class="shorttext synchronized" lang="en">Object descr. for list header (will be enhanced by obj. key)</p>
      "! @parameter iv_use_obj_name_as_is  | <p class="shorttext synchronized" lang="en">X = Use text in IV_OBJ_NAME as it is passed</p>
      "! @parameter iv_display_in_popup    | <p class="shorttext synchronized" lang="en">X = Display history in a popup</p>
      "! @parameter is_popup_corners       | <p class="shorttext synchronized" lang="en">Definition of the popup corner points/</p>
      "! @parameter iv_cnt_name            | <p class="shorttext synchronized" lang="en">Name of the ALV control/container</p>
      "! @raising   zcx_ca_appr_hist       | <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while handling approval history</p>
      constructor
        IMPORTING
          io_appr_hist           TYPE REF TO zcl_ca_appr_hist
          iv_for_last_cycle_only TYPE abap_boolean DEFAULT abap_true
          io_parent              TYPE REF TO cl_gui_container OPTIONAL
          iv_obj_name            TYPE lvc_title
          iv_use_obj_name_as_is  TYPE abap_boolean DEFAULT abap_false
          iv_display_in_popup    TYPE abap_boolean DEFAULT abap_false
          is_popup_corners       TYPE zca_s_scr_fw_popup_corners OPTIONAL
          iv_cnt_name            TYPE csequence OPTIONAL
        RAISING
          zcx_ca_appr_hist,

      process REDEFINITION.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Handler for approval history</p>
      mo_appr_hist           TYPE REF TO zcl_ca_appr_hist,

*     t a b l e s
      "! <p class="shorttext synchronized" lang="en">Approval history data prepared for output</p>
      mt_approval_list       TYPE zca_tt_approval_list,

*     s t r u c t u r e s
      "! <p class="shorttext synchronized" lang="en">Definition of the popup corner points</p>
      ms_popup_corners       TYPE zca_s_scr_fw_popup_corners,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Object descr. for list header (will be enhanced by obj. key)</p>
      mv_obj_name            TYPE lvc_title,
      "! <p class="shorttext synchronized" lang="en">X = Activate ALV filter to display only the last cycle</p>
      mv_for_last_cycle_only TYPE abap_boolean,
      "! <p class="shorttext synchronized" lang="en">X = Display history in a popup</p>
      mv_display_in_popup    TYPE abap_boolean,
      "! <p class="shorttext synchronized" lang="en">X = Use text in IV_OBJ_NAME as it is passed</p>
      mv_use_obj_name_as_is  TYPE abap_boolean.

*   i n s t a n c e   m e t h o d s
    METHODS:
      prepare_alv REDEFINITION.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.                     "zcl_ca_appr_hist_alv  DEFINITION


CLASS zcl_ca_appr_hist_alv IMPLEMENTATION.

  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    super->constructor( ir_table         = REF #( mt_approval_list )
                        io_container     = io_parent
                        iv_layout_handle = 'APHI'
                        iv_cnt_name      = iv_cnt_name ).

    mo_appr_hist           = io_appr_hist.
    mv_obj_name            = iv_obj_name.
    "Read all approvals and use filter functionality of the SALV
    mt_approval_list       = mo_appr_hist->get_approval_list_for_display( iv_for_last_cycle_only = abap_false ).
    mv_for_last_cycle_only = iv_for_last_cycle_only.
    ms_popup_corners       = is_popup_corners.
    mv_display_in_popup    = iv_display_in_popup.
    mv_use_obj_name_as_is  = iv_use_obj_name_as_is.
  ENDMETHOD.                    "constructor


  METHOD process.
    "-----------------------------------------------------------------*
    "   Controls the entire processing
    "-----------------------------------------------------------------*
    TRY.
        prepare_alv( ).

        IF mv_display_in_popup EQ abap_true.
          IF ms_popup_corners IS INITIAL.
            ms_popup_corners = VALUE #( starting_at_x = 10
                                        ending_at_x   = 180
                                        starting_at_y = 8
                                        ending_at_y   = 16 ).
          ENDIF.

          mo_salv->set_screen_popup( start_column = ms_popup_corners-starting_at_x
                                     end_column   = ms_popup_corners-ending_at_x
                                     start_line   = ms_popup_corners-starting_at_y
                                     end_line     = ms_popup_corners-ending_at_y ).
        ENDIF.

        mo_salv->display( ).

      CATCH cx_salv_error
            zcx_ca_error INTO DATA(lx_error).
        MESSAGE lx_error TYPE c_msgty_s DISPLAY LIKE c_msgty_e.
    ENDTRY.
  ENDMETHOD.                    "process


  METHOD prepare_alv.
    "-----------------------------------------------------------------*
    "   Do adjustment to ALV
    "-----------------------------------------------------------------*
    DATA(lo_result_values) = zcl_ca_c_approval_result=>get_instance( ).

    DATA(lo_display_settgs) = mo_salv->get_display_settings( ).
    lo_display_settgs->set_list_header_size( cl_salv_display_settings=>c_header_size_medium ).
    CASE mv_use_obj_name_as_is.
      WHEN abap_true.
        lo_display_settgs->set_list_header( mv_obj_name ).

      WHEN abap_false.
        DATA(lv_alv_header_txt) = zcl_ca_wf_utils=>prepare_object_key_for_ouput( mo_appr_hist->ms_bo_key ).
        lo_display_settgs->set_list_header( |{ mv_obj_name } { lv_alv_header_txt }| ).
    ENDCASE.

    "Deactivate all functions except filters
    DATA(lo_funcs) = mo_salv->get_functions( ).
    lo_funcs->set_all( abap_false ).
    lo_funcs->set_group_filter( ).
    lo_funcs->set_find( ).
    lo_funcs->set_find_more( ).

    IF mv_for_last_cycle_only EQ abap_true.
      DATA(lo_filters) = mo_salv->get_filters( ).
      lo_filters->add_filter( columnname = lo_result_values->column_name-approval_cycle
                              low        = CONV #( mo_appr_hist->mv_current_cycle ) ).
    ENDIF.

    "Set sorting and grouping
    DATA(lo_sort) = mo_salv->get_sorts( ).
    lo_sort->add_sort( columnname = lo_result_values->column_name-approval_cycle
                       sequence   = if_salv_c_sort=>sort_down
                       group      = if_salv_c_sort=>group_with_underline ).

    lo_sort->add_sort( columnname = lo_result_values->column_name-approval_level
                       sequence   = if_salv_c_sort=>sort_down ).

    lo_sort->add_sort( columnname = lo_result_values->column_name-approval_date
                       sequence   = if_salv_c_sort=>sort_down ).

    lo_sort->add_sort( columnname = lo_result_values->column_name-approval_time
                       sequence   = if_salv_c_sort=>sort_down ).

    LOOP AT mt_cols ASSIGNING FIELD-SYMBOL(<ls_col>).
      DATA(lo_col) = CAST cl_salv_column_table( <ls_col>-r_column ).
      CASE <ls_col>-columnname.
        WHEN lo_result_values->column_name-approval_cycle  OR
             lo_result_values->column_name-approval_level  OR
             lo_result_values->column_name-seqno.
          lo_col->set_key( ).
          lo_col->set_key_presence_required( ).
          lo_col->set_alignment( if_salv_c_alignment=>centered ).

        WHEN lo_result_values->column_name-icon_result.
          lo_col->set_icon( ).
          lo_col->set_alignment( if_salv_c_alignment=>centered ).

        WHEN lo_result_values->column_name-approval_date  OR
             lo_result_values->column_name-date_received.
          lo_col->set_alignment( if_salv_c_alignment=>centered ).

        WHEN lo_result_values->column_name-approval_time  OR
             lo_result_values->column_name-time_received.
          lo_col->set_edit_mask( '__:__' ).
          lo_col->set_alignment( if_salv_c_alignment=>centered ).

        WHEN lo_result_values->column_name-instid          OR
             lo_result_values->column_name-approval_result.
          lo_col->set_technical( ).

        WHEN lo_result_values->column_name-text_result.
          lo_col->set_visible( abap_false ).
      ENDCASE.
    ENDLOOP.

    "Hide cycle if only cycle was used
    IF mo_appr_hist->mv_current_cycle LE 1.
      mt_cols[ columnname = lo_result_values->column_name-approval_cycle ]-r_column->set_technical( ).
    ENDIF.

    "Hide sequence no. (is greater one only when more than one approver per level)
    LOOP AT mt_approval_list TRANSPORTING NO FIELDS
                             WHERE seqno GT 1.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      mt_cols[ columnname = lo_result_values->column_name-seqno ]-r_column->set_technical( ).
    ENDIF.
  ENDMETHOD.                    "prepare_alv

ENDCLASS.                     "zcl_ca_appr_hist_alv  IMPLEMENTATION


