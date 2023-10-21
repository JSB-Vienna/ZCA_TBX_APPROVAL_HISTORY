"! <p class="shorttext synchronized" lang="en">CA-TBX: Popup to display details of approval history</p>
CLASS zcl_ca_appr_hist_popup_details DEFINITION PUBLIC
                                                INHERITING FROM zcl_ca_reusable_popup_cust_cnt
                                                CREATE PUBLIC.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      "!
      "! @parameter io_appr_hist     | <p class="shorttext synchronized" lang="en">CA-TBX: Handler for approval history</p>
      "! @parameter iv_obj_name      | <p class="shorttext synchronized" lang="en">Object name as addition for the title</p>
      "! @raising   zcx_ca_appr_hist | <p class="shorttext synchronized" lang="en">Common exception: Error while handling approval history</p>
      constructor
        IMPORTING
          io_appr_hist TYPE REF TO zcl_ca_appr_hist
          iv_obj_name  TYPE text30
        RAISING
          zcx_ca_appr_hist.


* P R O T E C T E D   S E C T I O N
  PROTECTED SECTION.
*   i n s t a n c e   m e t h o d s
    METHODS:
      handle_pbo REDEFINITION,

      on_set_status REDEFINITION.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">CA-TBX: Handler for approval history</p>
      mo_appr_hist     TYPE REF TO zcl_ca_appr_hist,
      "! <p class="shorttext synchronized" lang="en">CA-TBX: ALV list with approval history</p>
      mo_alv_appr_list TYPE REF TO zcl_ca_appr_hist_alv,

*     s i n g l e   v a l u e s
      "! <p class="shorttext synchronized" lang="en">Object name as addition for the title</p>
      mv_obj_name      TYPE text30.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.


ENDCLASS.



CLASS ZCL_CA_APPR_HIST_POPUP_DETAILS IMPLEMENTATION.


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    DATA(lv_ending_at_x) = 130.
    super->constructor( iv_toolbar       = abap_false
                        is_popup_corners = VALUE #( starting_at_x = 10
                                                    starting_at_y = 8
                                                    ending_at_x   = lv_ending_at_x
                                                    ending_at_y   = 22 ) ).

    mo_appr_hist = io_appr_hist.
    mv_obj_name  = iv_obj_name.
  ENDMETHOD.                    "constructor


  METHOD handle_pbo.
    "-----------------------------------------------------------------*
    "   Handle event Process Before Output
    "-----------------------------------------------------------------*
    "Super method creates screen and custom container
    super->handle_pbo( iv_event ).

    IF mo_alv_appr_list IS BOUND.
      RETURN.
    ENDIF.

    mo_alv_appr_list = NEW #( io_parent    = mo_ccont_reuse
                              io_appr_hist = mo_appr_hist ).

    mo_alv_appr_list->process( ).
  ENDMETHOD.                    "handle_pbo


  METHOD on_set_status.
    "-----------------------------------------------------------------*
    "   Set GUI status
    "-----------------------------------------------------------------*
    super->on_set_status( io_gui_status ).

    io_gui_status->set_excl_fcode( VALUE #( ( mo_fcodes->save )
                                            ( mo_fcodes->cancel ) ) ).

    io_gui_status->set_titlebar(
            iv_titlebar_var1 = CONV #( TEXT-hdr )
            iv_titlebar_var2 = CONV #( mv_obj_name )
            iv_titlebar_var3 = CONV #( zcl_ca_wf_utils=>prepare_object_key_for_ouput( mo_appr_hist->ms_bo_key ) ) ).
  ENDMETHOD.                    "on_set_status
ENDCLASS.
