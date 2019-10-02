*&---------------------------------------------------------------------*
*& Report zpj_live_tweet
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zos_tweet_live.

PARAMETERS:

  p_user TYPE c LENGTH 15 LOWER CASE,
  p_name TYPE c LENGTH 50 LOWER CASE,
  p_text TYPE c LENGTH 255 LOWER CASE.

START-OF-SELECTION.

  TRY.
      DATA(lo_random) = cl_abap_random_int8=>create( seed = CONV #( sy-uzeit ) ).

      DATA(ls_tweet) = VALUE zos_cl_tweets=>gst_tweet(
        id        = lo_random->get_next( )
        text      = p_text
        truncated = abap_false
        user      = VALUE #(
          name        = p_user
          screen_name = p_name
        )
      ).

      DATA(lv_json) = /ui2/cl_json=>serialize(
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
        data        = ls_tweet
      ).

      DATA(lo_pcp) = cl_ac_message_type_pcp=>create( ).
      lo_pcp->set_text( lv_json ).

      CAST if_amc_message_producer_pcp( cl_amc_channel_manager=>create_message_producer(
        i_application_id = 'ZOS_TWEET_AMC'
        i_channel_id     = '/tweets'
      ) )->send( lo_pcp ).

    CATCH cx_amc_error  cx_ac_message_type_pcp_error INTO DATA(lo_ex).
      MESSAGE lo_ex->get_text( ) TYPE 'E'.

  ENDTRY.
