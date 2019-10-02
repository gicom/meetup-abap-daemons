CLASS zos_cl_tweet_daemon DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM cl_abap_daemon_ext_base.

  PUBLIC SECTION.

    INTERFACES:

      if_abap_timer_handler.

    CONSTANTS:

      cv_name TYPE classname VALUE 'ZOS_CL_TWEET_DAEMON'.

    CLASS-METHODS:

      start
        IMPORTING
          iv_name   TYPE char10
          iv_tweets TYPE i
        RAISING
          cx_abap_daemon_error
          cx_ac_message_type_pcp_error,

      stop
        IMPORTING
          iv_name TYPE char10
        RAISING
          cx_abap_daemon_error.

    METHODS:

      if_abap_daemon_extension~on_error REDEFINITION,

      if_abap_daemon_extension~on_message REDEFINITION,

      if_abap_daemon_extension~on_restart REDEFINITION,

      if_abap_daemon_extension~on_server_shutdown REDEFINITION,

      if_abap_daemon_extension~on_accept REDEFINITION,

      if_abap_daemon_extension~on_start REDEFINITION,

      if_abap_daemon_extension~on_stop REDEFINITION,

      if_abap_daemon_extension~on_system_shutdown REDEFINITION,

      if_abap_daemon_extension~on_before_restart_by_system REDEFINITION.

  PRIVATE SECTION.

    DATA:

      go_timer         TYPE REF TO if_abap_timer_manager,

      gv_tweets_left   TYPE i,

      go_random        TYPE REF TO cl_abap_random_int,

      gt_tweets        TYPE STANDARD TABLE OF zos_tweets,

      go_random_tweets TYPE REF TO cl_abap_random_int.

    METHODS:

      start_timer.

ENDCLASS.



CLASS zos_cl_tweet_daemon IMPLEMENTATION.

  METHOD if_abap_daemon_extension~on_accept.
    TRY.
        DATA(lv_my_program_name) = cl_oo_classname_service=>get_classpool_name( zos_cl_tweet_daemon=>cv_name ).

        IF i_context_base->get_start_caller_info( )-program = lv_my_program_name.
          e_setup_mode = co_setup_mode-accept.
        ELSE.
          e_setup_mode = co_setup_mode-reject.
        ENDIF.

      CATCH cx_abap_daemon_error.
        " Don't start on errors™
        e_setup_mode = co_setup_mode-reject.
    ENDTRY.
  ENDMETHOD.


  METHOD if_abap_daemon_extension~on_before_restart_by_system.

  ENDMETHOD.


  METHOD if_abap_daemon_extension~on_error.

  ENDMETHOD.


  METHOD if_abap_daemon_extension~on_message.

  ENDMETHOD.


  METHOD if_abap_daemon_extension~on_restart.

  ENDMETHOD.


  METHOD if_abap_daemon_extension~on_server_shutdown.

  ENDMETHOD.


  METHOD if_abap_daemon_extension~on_start.
    TRY.
        me->gv_tweets_left = i_context->get_start_parameter( )->get_field( 'tweets' ).

        me->go_random = cl_abap_random_int=>create(
          seed = CONV #( sy-uzeit )
          min  = 1
          max  = 100
        ).

        SELECT * FROM zos_tweets INTO TABLE @me->gt_tweets.

        me->go_random_tweets = cl_abap_random_int=>create(
          seed = me->go_random->get_next( )
          min  = 1
          max  = lines( me->gt_tweets )
        ).

        me->go_timer = cl_abap_timer_manager=>get_timer_manager( ).

        me->start_timer( ).

      CATCH cx_abap_daemon_error cx_abap_timer_error cx_ac_message_type_pcp_error.
        " No error handling :(

    ENDTRY.
  ENDMETHOD.


  METHOD if_abap_daemon_extension~on_stop.

  ENDMETHOD.


  METHOD if_abap_daemon_extension~on_system_shutdown.

  ENDMETHOD.


  METHOD if_abap_timer_handler~on_timeout.
    IF me->go_random->get_next( ) <= 10.
      ASSIGN me->gt_tweets[ me->go_random_tweets->get_next( ) ] TO FIELD-SYMBOL(<ls_tweet>).

      " Convert tweet to JSON format
      DATA(ls_tweet) = zos_cl_tweets=>convert_to_external( <ls_tweet> ).

      DATA(lv_json) = /ui2/cl_json=>serialize(
        data        = ls_tweet
        pretty_name = /ui2/cl_json=>pretty_mode-low_case
      ).

      " Send the message through the AMC channel
      TRY.
          DATA(lo_pcp) = cl_ac_message_type_pcp=>create( ).

          lo_pcp->set_text( lv_json ).

          CAST if_amc_message_producer_pcp( cl_amc_channel_manager=>create_message_producer(
            i_application_id = 'ZOS_TWEET_AMC'
            i_channel_id     = '/tweets'
          ) )->send( lo_pcp ).

        CATCH cx_amc_error cx_ac_message_type_pcp_error.
          " Sadness

      ENDTRY.

      me->gv_tweets_left = me->gv_tweets_left - 1.
    ENDIF.

    IF me->gv_tweets_left > 0.
      me->start_timer( ).
    ENDIF.
  ENDMETHOD.


  METHOD start_timer.
    TRY.
        me->go_timer->start_timer(
          i_timeout       = 100
          i_timer_handler = me
        ).

      CATCH cx_abap_timer_error.
        " No error handling :(
    ENDTRY.
  ENDMETHOD.


  METHOD start.
    DATA(lo_pcp) = cl_ac_message_type_pcp=>create( ).

    lo_pcp->set_field(
      i_name  = 'tweets'
      i_value = CONV #( iv_tweets )
    ).

    cl_abap_daemon_client_manager=>start(
      i_class_name = zos_cl_tweet_daemon=>cv_name
      i_name       = CONV #( iv_name )
      i_priority   = cl_abap_daemon_client_manager=>co_session_priority_low
      i_parameter  = lo_pcp
    ).
  ENDMETHOD.


  METHOD stop.
    DATA(lt_info) = cl_abap_daemon_client_manager=>get_daemon_info( zos_cl_tweet_daemon=>cv_name ).

    LOOP AT lt_info ASSIGNING FIELD-SYMBOL(<ls_info>) WHERE name = iv_name.
      cl_abap_daemon_client_manager=>stop( <ls_info>-instance_id ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
