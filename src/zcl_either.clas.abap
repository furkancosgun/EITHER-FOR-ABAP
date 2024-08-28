CLASS zcl_either DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS cond
      IMPORTING
        !condition       TYPE boolean
        !left_value      TYPE any
        !right_value     TYPE any
      RETURNING
        VALUE(ro_either) TYPE REF TO zcl_either
      RAISING
        zcx_either_invalid_type .
    CLASS-METHODS left
      IMPORTING
        !value           TYPE any
      RETURNING
        VALUE(ro_either) TYPE REF TO zcl_either
      RAISING
        zcx_either_invalid_type .
    CLASS-METHODS right
      IMPORTING
        !value           TYPE any
      RETURNING
        VALUE(ro_either) TYPE REF TO zcl_either
      RAISING
        zcx_either_invalid_type .
    METHODS is_left
      RETURNING
        VALUE(r_boolean) TYPE boolean .
    METHODS is_right
      RETURNING
        VALUE(r_boolean) TYPE boolean .
    METHODS get_left
      EXPORTING
        VALUE(value) TYPE any
      RAISING
        zcx_either_invalid_access .
    METHODS get_right
      EXPORTING
        VALUE(value) TYPE any
      RAISING
        zcx_either_invalid_access .
  PROTECTED SECTION.
    METHODS:
      get_value_from_ref
        IMPORTING
          ref   TYPE REF TO data
        EXPORTING
          value TYPE any
        RAISING
          zcx_either_invalid_type.
    METHODS:
      set_value_to_ref
        IMPORTING
          value TYPE any
        EXPORTING
          ref   TYPE REF TO data
        RAISING
          zcx_either_invalid_type.
ENDCLASS.



CLASS ZCL_EITHER IMPLEMENTATION.


  METHOD cond.
    ro_either = COND #( WHEN condition EQ abap_true THEN NEW zcl_either_right( right_value )  ELSE NEW zcl_either_left( left_value ) ).
  ENDMETHOD.


  METHOD get_left.
    IF NOT is_left( ).
      RAISE EXCEPTION TYPE zcx_either_invalid_access
        EXPORTING
          message = 'Invalid usage. You should check is_left before calling.'.
    ENDIF.

    CHECK value IS REQUESTED.

    me->get_value_from_ref(
      EXPORTING
        ref   = CAST zcl_either_left( me )->value
      IMPORTING
        value = value
    ).
  ENDMETHOD.


  METHOD get_right.
    IF NOT is_right( ).
      RAISE EXCEPTION TYPE zcx_either_invalid_access
        EXPORTING
          message = 'Invalid usage. You should check is_right before calling.'.
    ENDIF.

    CHECK value IS REQUESTED.

    me->get_value_from_ref(
      EXPORTING
        ref   = CAST zcl_either_right( me )->value
      IMPORTING
        value = value
    ).
  ENDMETHOD.


  METHOD get_value_from_ref.
    TRY.
        ASSIGN ref->* TO FIELD-SYMBOL(<fs_value>).
        value = <fs_value>.
      CATCH cx_root.
        RAISE EXCEPTION TYPE zcx_either_invalid_type
          EXPORTING
            message = 'Invalid type. The value must be a data type.'.
    ENDTRY.
  ENDMETHOD.


  METHOD is_left.
    r_boolean = boolc( me IS INSTANCE OF zcl_either_left ).
  ENDMETHOD.


  METHOD is_right.
    r_boolean = boolc( me IS INSTANCE OF zcl_either_right ).
  ENDMETHOD.


  METHOD left.
    ro_either = NEW zcl_either_left( value ).
  ENDMETHOD.


  METHOD right.
    ro_either = NEW zcl_either_right( value ).
  ENDMETHOD.


  METHOD set_value_to_ref.
    DATA(lo_typedescr) = cl_abap_datadescr=>describe_by_data( value ).
    DATA(lo_datadescr) = CAST cl_abap_datadescr( lo_typedescr ).

    IF lo_typedescr->type_kind EQ cl_abap_datadescr=>typekind_oref.
      RAISE EXCEPTION TYPE zcx_either_invalid_type
        EXPORTING
          message = 'Invalid type. The value must be a data type.'.
    ENDIF.

    CREATE DATA ref TYPE HANDLE lo_datadescr.

    ASSIGN ref->* TO FIELD-SYMBOL(<fs_value>).

    <fs_value> = value.
  ENDMETHOD.
ENDCLASS.
