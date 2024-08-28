CLASS lth_either_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS:
      test_cond_left FOR TESTING ,
      test_cond_right FOR TESTING ,
      test_get_left FOR TESTING ,
      test_get_right FOR TESTING ,
      test_is_left FOR TESTING ,
      test_is_right FOR TESTING ,
      test_invalid_get_left FOR TESTING ,
      test_invalid_get_right FOR TESTING ,
      test_cond_with_identical_value FOR TESTING ,
      test_cond_with_different_types FOR TESTING ,
      test_left_value_is_not_right FOR TESTING ,
      test_right_value_is_not_left FOR TESTING ,
      test_multiple_instances FOR TESTING ,
      test_get_value_with_large_data FOR TESTING .

ENDCLASS.

CLASS lth_either_test IMPLEMENTATION.

  METHOD test_cond_left.
    DATA:
      lv_value      TYPE string VALUE 'Left Value',
      lv_left_value TYPE string.

    DATA(lo_either) = zcl_either=>cond(
      condition   = abap_false
      left_value  = lv_value
      right_value = 'Right Value'
    ).

    cl_abap_unit_assert=>assert_true(
      act = lo_either->is_left( )
    ).

    lo_either->get_left(
      IMPORTING
        value = lv_left_value
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_left_value
      exp = lv_value
    ).
  ENDMETHOD.

  METHOD test_cond_right.
    DATA:lv_value TYPE string VALUE 'Right Value'.
    DATA:lv_right_value TYPE string.

    DATA(lo_either) = zcl_either=>cond(
      condition = abap_true
      left_value = 'Left Value'
      right_value = lv_value
    ).

    cl_abap_unit_assert=>assert_true(
      act = lo_either->is_right( )
    ).

    lo_either->get_right(
      IMPORTING
        value = lv_right_value
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_right_value
      exp = lv_value
    ).
  ENDMETHOD.

  METHOD test_get_left.
    DATA:lv_value TYPE string VALUE 'Left Value'.
    DATA:lv_retrieved_value TYPE string.

    DATA(lo_either) = zcl_either=>left( lv_value ).

    lo_either->get_left(
      IMPORTING
        value = lv_retrieved_value
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_retrieved_value
      exp = lv_value
    ).
  ENDMETHOD.

  METHOD test_get_right.
    DATA:lv_value TYPE string VALUE 'Right Value'.
    DATA:lv_retrieved_value TYPE string.

    DATA(lo_either) = zcl_either=>right( lv_value ).

    lo_either->get_right(
      IMPORTING
        value = lv_retrieved_value
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_retrieved_value
      exp = lv_value
    ).
  ENDMETHOD.

  METHOD test_invalid_get_left.
    DATA:lv_value TYPE string.

    DATA(lo_either) = zcl_either=>right( 'Right Value' ).

    TRY.
        lo_either->get_left(
          IMPORTING
            value = lv_value
        ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_either_invalid_access INTO DATA(e).
        cl_abap_unit_assert=>assert_not_initial(
          EXPORTING
            act              = e->get_text( )
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_invalid_get_right.
    DATA:lv_value TYPE string.

    DATA(lo_either) = zcl_either=>left( 'Left Value' ).

    TRY.
        lo_either->get_right(
          IMPORTING
            value = lv_value
        ).
        cl_abap_unit_assert=>fail( 'Expected exception not raised' ).
      CATCH zcx_either_invalid_access INTO DATA(e).
        cl_abap_unit_assert=>assert_not_initial(
          EXPORTING
            act              = e->get_text( )
        ).
    ENDTRY.
  ENDMETHOD.

  METHOD test_is_left.
    DATA(lo_either) = zcl_either=>left( 'Some Value' ).

    cl_abap_unit_assert=>assert_true(
      act = lo_either->is_left( )
    ).
  ENDMETHOD.

  METHOD test_is_right.
    DATA(lo_either) = zcl_either=>right( 'Some Value' ).

    cl_abap_unit_assert=>assert_true(
      act = lo_either->is_right( )
    ).
  ENDMETHOD.

  METHOD test_cond_with_identical_value.
    DATA:lv_value TYPE string VALUE 'Identical Value'.

    DATA(lo_either_left) = zcl_either=>left( lv_value ).
    DATA(lo_either_right) = zcl_either=>right( lv_value ).

    DATA(lo_cond) = zcl_either=>cond(
      condition = abap_false
      left_value = lv_value
      right_value = lv_value
    ).

    cl_abap_unit_assert=>assert_true(
      act = lo_cond->is_left( )
    ).

    DATA:lv_left_value TYPE string.
    lo_cond->get_left(
      IMPORTING
        value = lv_left_value
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_left_value
      exp = lv_value
    ).
  ENDMETHOD.

  METHOD test_cond_with_different_types.
    DATA:lv_string_value  TYPE string VALUE 'String Value',
         lv_integer_value TYPE i VALUE 42.

    DATA(lo_cond_string) = zcl_either=>cond(
      condition = abap_false
      left_value = lv_string_value
      right_value = lv_integer_value
    ).

    DATA:lv_retrieved_value TYPE string.

    lo_cond_string->get_left(
      IMPORTING
        value = lv_retrieved_value
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_retrieved_value
      exp = lv_string_value
    ).

  ENDMETHOD.

  METHOD test_left_value_is_not_right.
    DATA:lv_left_value  TYPE string VALUE 'Left Value',
         lv_right_value TYPE string VALUE 'Right Value'.

    DATA(lo_either_left) = zcl_either=>left( lv_left_value ).
    DATA(lo_either_right) = zcl_either=>right( lv_right_value ).

    cl_abap_unit_assert=>assert_true(
      act = lo_either_left->is_left( )
    ).

    cl_abap_unit_assert=>assert_false(
      act = lo_either_left->is_right( )
    ).
  ENDMETHOD.

  METHOD test_right_value_is_not_left.
    DATA:lv_left_value  TYPE string VALUE 'Left Value',
         lv_right_value TYPE string VALUE 'Right Value'.

    DATA(lo_either_right) = zcl_either=>right( lv_right_value ).
    DATA(lo_either_left) = zcl_either=>left( lv_left_value ).

    cl_abap_unit_assert=>assert_true(
      act = lo_either_right->is_right( )
    ).

    cl_abap_unit_assert=>assert_false(
      act = lo_either_right->is_left( )
    ).
  ENDMETHOD.

  METHOD test_multiple_instances.
    DATA:lv_left_value  TYPE string VALUE 'Left Value',
         lv_right_value TYPE string VALUE 'Right Value'.

    DATA(lo_either1) = zcl_either=>left( lv_left_value ).
    DATA(lo_either2) = zcl_either=>right( lv_right_value ).

    cl_abap_unit_assert=>assert_true(
      act = lo_either1->is_left( )
    ).

    cl_abap_unit_assert=>assert_true(
      act = lo_either2->is_right( )
    ).

    DATA:lv_retrieved_left_value TYPE string.
    DATA:lv_retrieved_right_value TYPE string.

    lo_either1->get_left(
      IMPORTING
        value = lv_retrieved_left_value
    ).

    lo_either2->get_right(
      IMPORTING
        value = lv_retrieved_right_value
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_retrieved_left_value
      exp = lv_left_value
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_retrieved_right_value
      exp = lv_right_value
    ).
  ENDMETHOD.

  METHOD test_get_value_with_large_data.
    DATA:lv_large_string    TYPE string,
         lv_retrieved_value TYPE string.
    DO 10000 TIMES.
      lv_large_string = |{ lv_large_string }X|.
    ENDDO.
    DATA(lo_either) = zcl_either=>left( lv_large_string ).

    lo_either->get_left(
      IMPORTING
        value = lv_retrieved_value
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_retrieved_value
      exp = lv_large_string
    ).

    DATA:lv_large_number TYPE i VALUE 999999.
    DATA:lv_retrieved_number TYPE i.

    DATA(lo_either_number) = zcl_either=>right( lv_large_number ).

    lo_either_number->get_right(
      IMPORTING
        value = lv_retrieved_number
    ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_retrieved_number
      exp = lv_large_number
    ).
  ENDMETHOD.

ENDCLASS.
