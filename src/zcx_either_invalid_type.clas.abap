CLASS zcx_either_invalid_type DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      constructor
        IMPORTING
          previous LIKE previous OPTIONAL
          message  TYPE string.
  PRIVATE SECTION.
    DATA:message TYPE string.
ENDCLASS.



CLASS zcx_either_invalid_type IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( previous = previous ).
    me->message = message.
  ENDMETHOD.
ENDCLASS.
