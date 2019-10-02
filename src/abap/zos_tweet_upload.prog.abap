*&---------------------------------------------------------------------*
*& Report zpj_upload_tweets
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zos_tweet_upload.

PARAMETERS:
  p_file TYPE localfile OBLIGATORY,
  p_dele AS CHECKBOX DEFAULT abap_true.

TYPES:

  BEGIN OF lst_tweets,
    tweets TYPE zos_cl_tweets=>gtt_tweet,
  END OF lst_tweets.

CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:

      select_file
        RETURNING
          VALUE(rv_file) TYPE localfile,

      read_file
        IMPORTING
          iv_file          TYPE localfile
        RETURNING
          VALUE(rs_tweets) TYPE lst_tweets.

ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.

  METHOD select_file.
    DATA:
      lt_filetable TYPE filetable,
      lv_subrc     TYPE i.

    CLEAR lt_filetable.

    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title = 'Choose file to upload'
        file_filter = '*.json'
        multiselection = abap_false
      CHANGING
        file_table = lt_filetable
        rc         = lv_subrc
    ).

    IF lv_subrc = 1.
      rv_file = lt_filetable[ 1 ].
    ENDIF.
  ENDMETHOD.


  METHOD read_file.
    DATA lt_data TYPE STANDARD TABLE OF string.

    cl_gui_frontend_services=>gui_upload(
      EXPORTING
        filename = CONV #( iv_file )
      CHANGING
        data_tab = lt_data
    ).

    /ui2/cl_json=>deserialize(
      EXPORTING
       json        = concat_lines_of( lt_data )
       pretty_name = /ui2/cl_json=>pretty_mode-low_case
      CHANGING
       data        = rs_tweets
    ).
  ENDMETHOD.

ENDCLASS.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  p_file = lcl_handler=>select_file( ).


START-OF-SELECTION.

  DATA(ls_json) = lcl_handler=>read_file( p_file ).

  DATA lt_tweets TYPE STANDARD TABLE OF zos_tweets.

  lt_tweets = VALUE #( FOR <ls_> IN ls_json-tweets ( zos_cl_tweets=>convert_to_internal( <ls_> ) ) ).

  IF p_dele = abap_true.
    DELETE FROM zos_tweets.
  ENDIF.

  INSERT zos_tweets FROM TABLE lt_tweets ACCEPTING DUPLICATE KEYS.

  IF sy-subrc = 0.
    MESSAGE |Inserted { sy-dbcnt } tweets!| TYPE 'S'.
  ELSE.
    MESSAGE |Did not work...| TYPE 'E'.
  ENDIF.






  " Here be dragons, don't scroll down further
