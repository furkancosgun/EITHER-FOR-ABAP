# EITHER FOR ABAP

`EITHER FOR ABAP` is a design pattern used in ABAP programming to standardize the management of results and error messages. This pattern helps in handling operations that can either succeed with a result or fail with an error message.

## License

This project is licensed under the [MIT License](LICENSE). See the LICENSE file for details.

## Contents

- [Overview](#overview)
- [Pattern Definition](#pattern-definition)
- [Method Examples](#method-examples)
- [Usage Example](#usage-example)

### Overview

The `Either` pattern represents the outcome of an operation with two possible results: a successful result (`right`) or an error message (`left`). This pattern enhances code readability and error handling by encapsulating results and errors in a consistent manner.

### Pattern Definition

The `Either` pattern provides a structure that can carry two types of values:

1. **Left**: Represents error messages or failure results.
2. **Right**: Represents successful results.

Below is a simplified definition of the `zcl_either` class:

```abap
CLASS zcl_either DEFINITION.
  PUBLIC SECTION.

      " Creates an instance based on a condition
      CLASS-METHODS cond
        IMPORTING
          !condition       TYPE boolean        " Condition to determine result
          !left_value      TYPE any            " Value for the left (error)
          !right_value     TYPE any            " Value for the right (success)
        RETURNING
          VALUE(ro_either) TYPE REF TO zcl_either
        RAISING
          zcx_either_invalid_type.              " Exception for invalid types

      " Creates an instance with a left value (error)
      CLASS-METHODS left
        IMPORTING
          !value           TYPE any            " Error result
        RETURNING
          VALUE(ro_either) TYPE REF TO zcl_either
        RAISING
          zcx_either_invalid_type.              " Exception for invalid types

      " Creates an instance with a right value (success)
      CLASS-METHODS right
        IMPORTING
          !value           TYPE any            " Success result
        RETURNING
          VALUE(ro_either) TYPE REF TO zcl_either
        RAISING
          zcx_either_invalid_type.              " Exception for invalid types

    " Checks if the instance is a left value
    METHODS is_left
      RETURNING
        VALUE(r_boolean) TYPE boolean.          " Returns TRUE if left value

    " Checks if the instance is a right value
    METHODS is_right
      RETURNING
        VALUE(r_boolean) TYPE boolean.          " Returns TRUE if right value

    " Retrieves the left value
    METHODS get_left
      EXPORTING
        VALUE(value) TYPE any                   " Retrieves left value
      RAISING
        zcx_either_invalid_access.             " Exception for invalid access

    " Retrieves the right value
    METHODS get_right
      EXPORTING
        VALUE(value) TYPE any                   " Retrieves right value
      RAISING
        zcx_either_invalid_access.             " Exception for invalid access
ENDCLASS.
```

### Method Examples

Here are some examples of how the `zcl_either` class can be used:

1. **`divide_numbers` Method**: Divides two numbers and returns the result or an error message if division by zero is attempted.

   ```abap
   METHOD divide_numbers.
     IF iv_denominator = 0.
       ro_result = zcl_either=>left( 'Division by zero error' ).
     ELSE.
       DATA(lv_result) = iv_numerator / iv_denominator.
       ro_result = zcl_either=>right( lv_result ).
     ENDIF.
   ENDMETHOD.
   ```

2. **`fetch_data` Method**: Retrieves data based on an ID and returns the data or an error message if the ID is invalid.

   ```abap
   METHOD fetch_data.
     IF iv_id < 1.
       ro_result = zcl_either=>left( 'Invalid ID' ).
     ELSE.
       DATA(lv_data) = 'Fetched Data'.
       ro_result = zcl_either=>right( lv_data ).
     ENDIF.
   ENDMETHOD.
   ```

3. **`process_order` Method**: Processes an order based on the order ID and returns a success or error message.

   ```abap
   METHOD process_order.
     CASE iv_order_id.
       WHEN 1.
         ro_result = zcl_either=>right( 'Order processed successfully' ).
       WHEN 2.
         ro_result = zcl_either=>left( 'Order already processed' ).
       WHEN OTHERS.
         ro_result = zcl_either=>left( 'Order not found' ).
     ENDCASE.
   ENDMETHOD.
   ```

4. **`complex_operation` Method**: Performs a complex operation and handles errors based on the input.

   ```abap
   METHOD complex_operation.
     DATA: lv_intermediate TYPE i.
     IF iv_input < 0.
       ro_result = zcl_either=>left( 'Negative input not allowed' ).
     ELSEIF iv_input = 0.
       ro_result = zcl_either=>left( 'Zero input not valid' ).
     ELSE.
       lv_intermediate = iv_input * 2.
       ro_result = zcl_either=>right( lv_intermediate ).
     ENDIF.
   ENDMETHOD.
   ```

### Usage Example

Here’s an example of how to use the `zcl_either` class in practice:

```abap
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
    lo_result->get_right( IMPORTING value = lv_result ).
    WRITE: / 'Data from fetch_data:', lv_result.
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
```

Feel free to adjust these suggestions based on your specific needs or the context of your project!
