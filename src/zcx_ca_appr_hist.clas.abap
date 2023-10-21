"! <p class="shorttext synchronized" lang="en">CA-TBX exception: Error while handling approval history</p>
class ZCX_CA_APPR_HIST definition
  public
  inheriting from ZCX_CA_PARAM
  create public .

public section.

  constants:
    BEGIN OF zcx_ca_appr_hist,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '055',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF zcx_ca_appr_hist .
  constants:
    BEGIN OF bo_not_exist,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '048',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF bo_not_exist .
  constants:
    BEGIN OF wrong_next_lvl,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '056',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE 'MV_MSGV2',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF wrong_next_lvl .
  constants:
    BEGIN OF curr_no_entries,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '057',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF curr_no_entries .
  constants:
    BEGIN OF no_history_available,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '059',
        attr1 TYPE scx_attrname VALUE '',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF no_history_available .
  constants:
    BEGIN OF last_level_achieved,
        msgid TYPE symsgid VALUE 'ZCA_TOOLBOX',
        msgno TYPE symsgno VALUE '060',
        attr1 TYPE scx_attrname VALUE 'MV_MSGV1',
        attr2 TYPE scx_attrname VALUE '',
        attr3 TYPE scx_attrname VALUE '',
        attr4 TYPE scx_attrname VALUE '',
      END OF last_level_achieved .
  constants:
    begin of APPROVAL_ENTRY_NOT_FOUND,
      msgid type symsgid value 'ZCA_TOOLBOX',
      msgno type symsgno value '089',
      attr1 type scx_attrname value 'MV_MSGV1',
      attr2 type scx_attrname value 'MV_MSGV2',
      attr3 type scx_attrname value 'MV_MSGV3',
      attr4 type scx_attrname value '',
    end of APPROVAL_ENTRY_NOT_FOUND .
    "! <p class="shorttext synchronized" lang="en">My own name</p>
  constants C_ZCX_CA_APPR_HIST type SEOCLSNAME value 'ZCX_CA_APPR_HIST' ##NO_TEXT.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MT_RETURN type BAPIRET2_T optional
      !MV_SUBRC type SYST_SUBRC optional
      !MV_MSGTY type SYMSGTY optional
      !MV_MSGV1 type SYMSGV optional
      !MV_MSGV2 type SYMSGV optional
      !MV_MSGV3 type SYMSGV optional
      !MV_MSGV4 type SYMSGV optional
      !MV_SEVERITY type T_SEVERITY optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCX_CA_APPR_HIST IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MT_RETURN = MT_RETURN
MV_SUBRC = MV_SUBRC
MV_MSGTY = MV_MSGTY
MV_MSGV1 = MV_MSGV1
MV_MSGV2 = MV_MSGV2
MV_MSGV3 = MV_MSGV3
MV_MSGV4 = MV_MSGV4
MV_SEVERITY = MV_SEVERITY
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = ZCX_CA_APPR_HIST .
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
