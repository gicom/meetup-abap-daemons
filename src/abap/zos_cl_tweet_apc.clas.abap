CLASS zos_cl_tweet_apc DEFINITION
  PUBLIC
  INHERITING FROM cl_apc_wsp_ext_stateful_pcp_b
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    DATA:

      go_message_manager TYPE REF TO if_apc_wsp_message_manager_pcp.

    INTERFACES:

      if_amc_message_receiver_pcp.

    METHODS:

      if_apc_wsp_ext_pcp~on_message REDEFINITION,

      if_apc_wsp_ext_pcp~on_start REDEFINITION.

ENDCLASS.



CLASS zos_cl_tweet_apc IMPLEMENTATION.

  METHOD if_apc_wsp_ext_pcp~on_message.
    " We just play ping pong
    TRY.
        DATA(lo_pcp) = i_message_manager->create_message( ).
        lo_pcp->set_text( i_message->get_text( ) ).

        i_message_manager->send( lo_pcp ).

      CATCH cx_apc_error cx_ac_message_type_pcp_error.
        " :(

    ENDTRY.
  ENDMETHOD.


  METHOD if_apc_wsp_ext_pcp~on_start.
    me->go_message_manager = i_message_manager.

    " Bind to AMC channel
    TRY.
        cl_amc_channel_manager=>create_message_consumer(
          i_application_id = 'ZOS_TWEET_AMC'
          i_channel_id     = '/tweets'
        )->start_message_delivery( me ).

      CATCH cx_amc_error.
        " Engage maximum sadness mode
    ENDTRY.
  ENDMETHOD.


  METHOD if_amc_message_receiver_pcp~receive.
    " Forward message to push channel
    TRY.
        DATA(lo_pcp) = me->go_message_manager->create_message( ).
        lo_pcp->set_text( i_message->get_text( ) ).

        me->go_message_manager->send( lo_pcp ).

      CATCH cx_apc_error cx_ac_message_type_pcp_error.
        "handle exception
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
