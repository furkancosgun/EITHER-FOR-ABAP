CLASS zcl_either_right DEFINITION
  PUBLIC
  INHERITING FROM zcl_either
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS
      constructor
        IMPORTING
          value TYPE any
        RAISING
          zcx_either_invalid_type.

    DATA:value TYPE REF TO data READ-ONLY.
ENDCLASS.



CLASS ZCL_EITHER_RIGHT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).
    me->set_value_to_ref( EXPORTING value = value IMPORTING ref = me->value ).
  ENDMETHOD.
ENDCLASS.
