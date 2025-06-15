*"* use this source file for your ABAP unit test classes

CLASS ltc_add_approver DEFINITION DEFERRED.
CLASS zcl_ca_appr_hist DEFINITION LOCAL FRIENDS ltc_add_approver.

*---------------------------------------------------------------------*
*     CLASS  ltc_add_approver  DEFINITION
*---------------------------------------------------------------------*
"! <p class="shorttext synchronized" lang="en">Test class: Add new approver to a level</p>
CLASS ltc_add_approver DEFINITION
                            INHERITING FROM cl_aunit_assert
                            FOR TESTING RISK LEVEL HARMLESS
                                        DURATION   SHORT.
  PUBLIC SECTION.


  PRIVATE SECTION.
    TYPES ty_t_approver TYPE SORTED TABLE OF swhactor WITH UNIQUE KEY primary_key COMPONENTS objid.

    CLASS-DATA:
      mo_db_mock           TYPE REF TO if_osql_test_environment.

    DATA:
      mo_cut               TYPE REF TO zcl_ca_appr_hist.

    CLASS-METHODS:
      class_setup,
      class_teardown.

    METHODS:
      setup,
      "! <p class="shorttext synchronized" lang="en">Add multiple approver to one level</p>
      add_multiple_approver_2_level FOR TESTING,
      "! <p class="shorttext synchronized" lang="en">Add multiple approver to multiple level</p>
      add_multi_apprvr_2_multi_level FOR TESTING,
      teardown,
      get_dialog_user_from_system
        IMPORTING
          iv_number_of_rows  TYPE i
        RETURNING
          VALUE(rt_approver) TYPE swfuagents.  "ty_t_approver.
ENDCLASS.                     "ltc_add_approver  DEFINITION


CLASS ltc_add_approver IMPLEMENTATION.

  METHOD class_setup.
    "-----------------------------------------------------------------*
    "   Check and preparation of test data
    "-----------------------------------------------------------------*
    mo_db_mock = cl_osql_test_environment=>create( VALUE #( ( 'ZCA_APPRHIST' ) ) ).
  ENDMETHOD.                    "class_setup


  METHOD setup.
    "-----------------------------------------------------------------*
    "   Prepare data for each testing method
    "-----------------------------------------------------------------*
    mo_cut = NEW #( VALUE #( instid = '1234567890'
                             typeid = 'ZCL_CA_APPR_HIST'
                             catid  = swfco_objtype_cl ) ) ##no_text.
  ENDMETHOD.                    "setup


  METHOD add_multiple_approver_2_level.
    "-----------------------------------------------------------------*
    "   Case/Given: Add multiple approver to one level
    "   Expected result: Sequence number equals number of rows
    "-----------------------------------------------------------------*
    "GIVEN ............................................................
    mo_cut->increase_approval_level( ).
    DATA(lt_approver) = get_dialog_user_from_system( 4 ).

    "WHEN .............................................................
    LOOP AT lt_approver REFERENCE INTO DATA(lr_approver).
      DATA(ls_approval_key) = mo_cut->add_approver( iv_approver = lr_approver->objid ).
    ENDLOOP.

    "THEN .............................................................
    assert_equals(  exp = CONV tprlfdnr( lines( lt_approver ) )
                    act = ls_approval_key-seqno ).
  ENDMETHOD.                    "add_multiple_approver_2_level


  METHOD add_multi_apprvr_2_multi_level.
    "-----------------------------------------------------------------*
    "   Case/Given: Add multiple approver to multiple level
    "   Expected result: Sequence number equals number of rows
    "-----------------------------------------------------------------*
    "Local data definitions
    DATA:
      lt_approver_4_level  TYPE swfuagents.

    "GIVEN ............................................................
    DATA(lt_approver) = get_dialog_user_from_system( 15 ).

    "WHEN .............................................................
    DO 3 TIMES.
      mo_cut->increase_approval_level( ).

      CLEAR lt_approver_4_level.
      LOOP AT lt_approver INTO DATA(ls_approver) TO 5.
        APPEND ls_approver TO lt_approver_4_level.
      ENDLOOP.
      DELETE lt_approver TO 5.


      LOOP AT lt_approver_4_level REFERENCE INTO DATA(lr_approver_4_level).
        DATA(ls_approval_key) = mo_cut->add_approver( iv_approver = lr_approver_4_level->objid ).
      ENDLOOP.

      "THEN .............................................................
      assert_equals(  exp = CONV tprlfdnr( lines( lt_approver_4_level ) )
                      act = ls_approval_key-seqno ).
    ENDDO.
  ENDMETHOD.                    "add_multi_apprvr_2_multi_level


  METHOD teardown.
    "-----------------------------------------------------------------*
    "   Release and my be delete single test data
    "-----------------------------------------------------------------*
    mo_db_mock->clear_doubles( ).
  ENDMETHOD.                    "teardown


  METHOD class_teardown.
    "-----------------------------------------------------------------*
    "   Deletion of test data and settings
    "-----------------------------------------------------------------*
    mo_db_mock->destroy( ).
    CLEAR mo_db_mock.
  ENDMETHOD.                    "class_teardown


  METHOD get_dialog_user_from_system.
    "-----------------------------------------------------------------*
    "   Select randomly dialog users for testing
    "-----------------------------------------------------------------*
    SELECT FROM usr02
         FIELDS @swfco_org_user AS otype,
                bname AS objid
          WHERE gltgv LE @sy-datlo
            AND gltgb GE @sy-datlo
            AND ustyp EQ 'A'    "Dialog user
           INTO TABLE @rt_approver
                UP TO @iv_number_of_rows ROWS.
  ENDMETHOD.                    "get_dialog_user_from_system

ENDCLASS.                     "ltc_add_approver  IMPLEMENTATION
