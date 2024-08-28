*&---------------------------------------------------------------------*
*& Report ZABAP_EITHER_DEMO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zabap_either_demo.
CLASS zcl_either_example DEFINITION.
  PUBLIC SECTION.
    " Method to divide two numbers
    METHODS:
      divide_numbers
        IMPORTING
          iv_numerator     TYPE i " Dividend
          iv_denominator   TYPE i " Divisor
        RETURNING
          VALUE(ro_result) TYPE REF TO zcl_either. " Result of division, wrapped in zcl_either

    " Method to fetch data based on an ID
    METHODS:
      fetch_data
        IMPORTING
          iv_id            TYPE i " Identifier for fetching data
        RETURNING
          VALUE(ro_result) TYPE REF TO zcl_either. " Fetched data or error message, wrapped in zcl_either

    " Method to process an order based on order ID
    METHODS:
      process_order
        IMPORTING
          iv_order_id      TYPE i " Order ID for processing
        RETURNING
          VALUE(ro_result) TYPE REF TO zcl_either. " Result of order processing or error message, wrapped in zcl_either

    " Method for a complex operation
    METHODS:
      complex_operation
        IMPORTING
          iv_input         TYPE i " Input for the operation
        RETURNING
          VALUE(ro_result) TYPE REF TO zcl_either. " Result of the operation or error message, wrapped in zcl_either
ENDCLASS.

CLASS zcl_either_example IMPLEMENTATION.
  METHOD divide_numbers.
    " Check if the denominator is zero
    IF iv_denominator = 0.
      " Return an error if dividing by zero
      ro_result = zcl_either=>left( 'Division by zero error' ).
    ELSE.
      " Perform the division and return the result
      DATA(lv_result) = iv_numerator / iv_denominator.
      ro_result = zcl_either=>right( lv_result ).
    ENDIF.
  ENDMETHOD.

  METHOD fetch_data.
    " Check if the ID is valid
    IF iv_id < 1.
      " Return an error if the ID is invalid
      ro_result = zcl_either=>left( 'Invalid ID' ).
    ELSE.
      " Simulate data fetching and return the data
      DATA(lv_data) = 'Fetched Data'.
      ro_result = zcl_either=>right( lv_data ).
    ENDIF.
  ENDMETHOD.

  METHOD process_order.
    " Process the order based on the order ID
    CASE iv_order_id.
      WHEN 1.
        " Return a success message for a specific order ID
        ro_result = zcl_either=>right( 'Order processed successfully' ).
      WHEN 2.
        " Return an error message if the order was already processed
        ro_result = zcl_either=>left( 'Order already processed' ).
      WHEN OTHERS.
        " Return an error message for unknown order IDs
        ro_result = zcl_either=>left( 'Order not found' ).
    ENDCASE.
  ENDMETHOD.

  METHOD complex_operation.
    DATA: lv_intermediate TYPE i.
    " Check if the input is valid
    IF iv_input < 0.
      " Return an error if the input is negative
      ro_result = zcl_either=>left( 'Negative input not allowed' ).
    ELSEIF iv_input = 0.
      " Return an error if the input is zero
      ro_result = zcl_either=>left( 'Zero input not valid' ).
    ELSE.
      " Perform a complex operation and return the result
      lv_intermediate = iv_input * 2.
      ro_result = zcl_either=>right( lv_intermediate ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA: lo_example TYPE REF TO zcl_either_example,
        lo_result  TYPE REF TO zcl_either,
        lv_result  TYPE i,
        lv_error   TYPE string.

  " Create an instance of the class
  lo_example = NEW zcl_either_example( ).

  " Test Example-1: divide_numbers
  lo_result = lo_example->divide_numbers(
    iv_numerator = 10
    iv_denominator = 0
  ).

  IF lo_result->is_left( ).
    " Retrieve and display the error message
    lo_result->get_left( IMPORTING value = lv_error ).
    WRITE: / 'Error in divide_numbers:', lv_error.
  ELSE.
    " Retrieve and display the result
    lo_result->get_right( IMPORTING value = lv_result ).
    WRITE: / 'Result of divide_numbers:', lv_result.
  ENDIF.

  " Test Example-2: fetch_data
  lo_result = lo_example->fetch_data(
    iv_id = 0
  ).

  IF lo_result->is_left( ).
    " Retrieve and display the error message
    lo_result->get_left( IMPORTING value = lv_error ).
    WRITE: / 'Error in fetch_data:', lv_error.
  ELSE.
    " Retrieve and display the data
    lo_result->get_right( IMPORTING value = lv_error ).
    WRITE: / 'Data from fetch_data:', lv_error.
  ENDIF.

  " Test Example-3: process_order
  lo_result = lo_example->process_order(
    iv_order_id = 2
  ).

  IF lo_result->is_left( ).
    " Retrieve and display the error message
    lo_result->get_left( IMPORTING value = lv_error ).
    WRITE: / 'Error in process_order:', lv_error.
  ELSE.
    " Retrieve and display the success message
    lo_result->get_right( IMPORTING value = lv_error ).
    WRITE: / 'Success message from process_order:', lv_error.
  ENDIF.

  " Test Example-4: complex_operation
  lo_result = lo_example->complex_operation(
    iv_input = 5
  ).

  IF lo_result->is_left( ).
    " Retrieve and display the error message
    lo_result->get_left( IMPORTING value = lv_error ).
    WRITE: / 'Error in complex_operation:', lv_error.
  ELSE.
    " Retrieve and display the result
    lo_result->get_right( IMPORTING value = lv_result ).
    WRITE: / 'Result of complex_operation:', lv_result.
  ENDIF.
