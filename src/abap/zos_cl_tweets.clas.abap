CLASS zos_cl_tweets DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:

      BEGIN OF gst_tweet,
        id        TYPE int8,
        text      TYPE string,
        BEGIN OF user,
          name              TYPE string,
          screen_name       TYPE string,
          profile_image_url TYPE string,
        END OF user,
        truncated TYPE abap_bool,
        BEGIN OF extended_tweet,
          full_text TYPE string,
        END OF extended_tweet,
      END OF gst_tweet,

      gtt_tweet TYPE STANDARD TABLE OF gst_tweet WITH DEFAULT KEY.

    CLASS-METHODS:

      convert_to_internal
        IMPORTING
          is_tweet        TYPE zos_cl_tweets=>gst_tweet
        RETURNING
          VALUE(rs_tweet) TYPE zos_tweets,

      convert_to_external
        IMPORTING
          is_tweet        TYPE zos_tweets
        RETURNING
          VALUE(rs_tweet) TYPE zos_cl_tweets=>gst_tweet.

ENDCLASS.



CLASS zos_cl_tweets IMPLEMENTATION.

  METHOD convert_to_external.
    rs_tweet = VALUE #(
      id        = is_tweet-id
      text      = is_tweet-text
      truncated = abap_false
      user      = VALUE #(
        name              = is_tweet-user_name
        screen_name       = is_tweet-user_screen_name
        profile_image_url = is_tweet-user_profile_image_url
      )
    ).
  ENDMETHOD.


  METHOD convert_to_internal.
    rs_tweet = VALUE #(
      id                     = is_tweet-id
      text                   = COND #( WHEN is_tweet-truncated = abap_true THEN is_tweet-extended_tweet-full_text ELSE is_tweet-text )
      user_name              = is_tweet-user-name
      user_screen_name       = is_tweet-user-screen_name
      user_profile_image_url = is_tweet-user-profile_image_url
    ).
  ENDMETHOD.

ENDCLASS.
