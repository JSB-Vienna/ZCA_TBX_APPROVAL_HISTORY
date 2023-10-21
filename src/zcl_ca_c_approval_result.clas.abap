"! <p class="shorttext synchronized" lang="en">CA-TBX: Constants for approval history result</p>
CLASS zcl_ca_c_approval_result DEFINITION PUBLIC
                                          CREATE PROTECTED.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Approval result: Technical values</p>
      BEGIN OF approval_result,
        "! <p class="shorttext synchronized" lang="en">Approval result: Open</p>
        open     TYPE swf_appres VALUE space,
        "! <p class="shorttext synchronized" lang="en">Approval result: Approved</p>
        approved TYPE swf_appres VALUE '0' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Approval result: Rejected</p>
        rejected TYPE swf_appres VALUE '4' ##no_text,
      END   OF approval_result,

      "! <p class="shorttext synchronized" lang="en">Column names for approval list</p>
      BEGIN OF column_name,
        catid               TYPE fieldname         VALUE 'CATID' ##no_text,
        typeid              TYPE fieldname         VALUE 'TYPEID' ##no_text,
        instid              TYPE fieldname         VALUE 'INSTID' ##no_text,
        approval_cycle      TYPE fieldname         VALUE 'APPROVAL_CYCLE' ##no_text,
        approval_level      TYPE fieldname         VALUE 'APPROVAL_LEVEL' ##no_text,
        seqno               TYPE fieldname         VALUE 'SEQNO' ##no_text,
        approval_result     TYPE fieldname         VALUE 'APPROVAL_RESULT' ##no_text,
        approving_user_id   TYPE fieldname         VALUE 'APPROVING_USER_ID' ##no_text,
        approving_user_name TYPE fieldname         VALUE 'APPROVING_USER_NAME' ##no_text,
        approval_date       TYPE fieldname         VALUE 'APPROVAL_DATE' ##no_text,
        approval_time       TYPE fieldname         VALUE 'APPROVAL_TIME' ##no_text,
        receiving_user_id   TYPE fieldname         VALUE 'RECEIVING_USER_ID' ##no_text,
        receiving_user_name TYPE fieldname         VALUE 'RECEIVING_USER_NAME' ##no_text,
        date_received       TYPE fieldname         VALUE 'DATE_RECEIVED' ##no_text,
        time_received       TYPE fieldname         VALUE 'TIME_RECEIVED' ##no_text,
        icon_result         TYPE fieldname         VALUE 'ICON_RESULT' ##no_text,
        text_result         TYPE fieldname         VALUE 'TEXT_RESULT' ##no_text,
        reason              TYPE fieldname         VALUE 'REASON' ##no_text,
      END   OF column_name.

*   i n s t a n c e   a t t r i b u t e s
    DATA:
      "! <p class="shorttext synchronized" lang="en">Approval result: Textual values</p>
      BEGIN OF approval_text READ-ONLY,
        "! <p class="shorttext synchronized" lang="en">Approval result: Open</p>
        open     TYPE zca_d_text_result,
        "! <p class="shorttext synchronized" lang="en">Approval result: Approved</p>
        approved TYPE zca_d_text_result,
        "! <p class="shorttext synchronized" lang="en">Approval result: Rejected</p>
        rejected TYPE zca_d_text_result,
      END   OF approval_text,

      "! <p class="shorttext synchronized" lang="en">Approval result: Textual values</p>
      BEGIN OF result_icon READ-ONLY,
        "! <p class="shorttext synchronized" lang="en">Approval result: Open</p>
        open     TYPE zca_d_icon_result,
        "! <p class="shorttext synchronized" lang="en">Approval result: Approved</p>
        approved TYPE zca_d_icon_result,
        "! <p class="shorttext synchronized" lang="en">Approval result: Rejected</p>
        rejected TYPE zca_d_icon_result,
      END   OF result_icon.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_c_approval_result.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Constructor</p>
      constructor,

      "! <p class="shorttext synchronized" lang="en">Convert approval result into an icon with quick info</p>
      "!
      "! @parameter approval_result  | <p class="shorttext synchronized" lang="en">Approval result</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">Icon with quick info</p>
      "! @raising   zcx_ca_appr_hist | <p class="shorttext synchronized" lang="en">Common exception: Error while handling approval history</p>
      get_icon_for_result
        IMPORTING
          approval_result TYPE swf_appres
        RETURNING
          VALUE(result)   TYPE zca_d_icon_result
        RAISING
          zcx_ca_appr_hist,

      "! <p class="shorttext synchronized" lang="en">Convert approval result into a text</p>
      "!
      "! @parameter approval_result  | <p class="shorttext synchronized" lang="en">Approval result</p>
      "! @parameter result           | <p class="shorttext synchronized" lang="en">Text</p>
      "! @raising   zcx_ca_appr_hist | <p class="shorttext synchronized" lang="en">Common exception: Error while handling approval history</p>
      get_text_for_result
        IMPORTING
          approval_result TYPE swf_appres
        RETURNING
          VALUE(result)   TYPE zca_d_text_result
        RAISING
          zcx_ca_appr_hist,

      "! <p class="shorttext synchronized" lang="en">Valid approval result passed?</p>
      "!
      "! @parameter approval_result  | <p class="shorttext synchronized" lang="en">Approval result</p>
      "! @raising   zcx_ca_appr_hist | <p class="shorttext synchronized" lang="en">Common exception: Error while handling approval history</p>
      is_approval_result_valid FINAL
        IMPORTING
          approval_result TYPE swf_appres
        RAISING
          zcx_ca_appr_hist.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      singleton_instance     TYPE REF TO zcl_ca_c_approval_result.

ENDCLASS.



CLASS ZCL_CA_C_APPROVAL_RESULT IMPLEMENTATION.


  METHOD constructor.
    "-----------------------------------------------------------------*
    "   Constructor
    "-----------------------------------------------------------------*
    approval_text-open     = 'Open'(opn).
    approval_text-approved = 'Approved'(apr).
    approval_text-rejected = 'Rejected'(rej).

    result_icon-open     = zcl_ca_utils=>icon_create( iv_icon      = icon_initial
                                                      iv_quickinfo = 'Approval is open'(aio) ).
    result_icon-approved = zcl_ca_utils=>icon_create( iv_icon      = icon_allow
                                                      iv_quickinfo = 'Object is approved'(oap) ).
    result_icon-rejected = zcl_ca_utils=>icon_create( iv_icon      = icon_reject
                                                      iv_quickinfo = 'Object is rejected'(orj) ).
  ENDMETHOD.                    "constructor


  METHOD get_icon_for_result.
    "-----------------------------------------------------------------*
    "   Convert approval result into a icon with quick info
    "-----------------------------------------------------------------*
    is_approval_result_valid( approval_result ).

    CASE approval_result.
      WHEN me->approval_result-open.
        result = result_icon-open.

      WHEN me->approval_result-approved.
        result = result_icon-approved.

      WHEN me->approval_result-rejected.
        result = result_icon-rejected.
    ENDCASE.
  ENDMETHOD.                    "get_icon_for_result


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_c_approval_result=>singleton_instance IS NOT BOUND.
      zcl_ca_c_approval_result=>singleton_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_c_approval_result=>singleton_instance.
  ENDMETHOD.                    "get_instance


  METHOD get_text_for_result.
    "-----------------------------------------------------------------*
    "   Convert approval result into a text
    "-----------------------------------------------------------------*
    is_approval_result_valid( approval_result ).

    CASE approval_result.
      WHEN me->approval_result-open.
        result = approval_text-open.

      WHEN me->approval_result-approved.
        result = approval_text-approved.

      WHEN me->approval_result-rejected.
        result = approval_text-rejected.
    ENDCASE.
  ENDMETHOD.                    "get_text_for_result


  METHOD is_approval_result_valid.
    "-----------------------------------------------------------------*
    "   Valid approval result passed?
    "-----------------------------------------------------------------*
    IF approval_result NE me->approval_result-open     AND
       approval_result NE me->approval_result-approved AND
       approval_result NE me->approval_result-rejected.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_appr_hist
        EXPORTING
          textid   = zcx_ca_appr_hist=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_APPROVAL_RESULT'
          mv_msgv2 = CONV #( approval_result ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_approval_result_valid
ENDCLASS.
