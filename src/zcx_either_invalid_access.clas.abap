CLASS zcx_either_invalid_access DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS
      constructor
        IMPORTING
          message TYPE string.
    METHODS get_text REDEFINITION.
  PRIVATE SECTION.
    DATA:message TYPE string.
ENDCLASS.



CLASS zcx_either_invalid_access IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    me->message = message.
  ENDMETHOD.
  METHOD get_text.
    result = me->message.
  ENDMETHOD.
ENDCLASS.
