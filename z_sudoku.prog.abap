*&---------------------------------------------------------------------*
*& Report  Z_SUDOKU
*&
*&---------------------------------------------------------------------*
*& 20170728 Daniel Wegner
*&
*&---------------------------------------------------------------------*
REPORT z_sudoku.
CLASS lcl_sudoku DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES if_serializable_object.
    TYPES mty_gamename TYPE zwd_sudoku_gamename.
    CONSTANTS difficulty_easy TYPE i VALUE 1 ##NO_TEXT.
    CONSTANTS difficulty_medium TYPE i VALUE 2 ##NO_TEXT.
    CONSTANTS difficulty_hard TYPE i VALUE 3 ##NO_TEXT.
    CLASS-METHODS shlp_gamename RETURNING VALUE(rv_gamename) TYPE mty_gamename.
    CLASS-METHODS chck_gamename IMPORTING iv_gamename TYPE mty_gamename RETURNING VALUE(rv_exists) TYPE abap_bool.
    CLASS-METHODS load_savegame IMPORTING iv_gamename TYPE mty_gamename RETURNING VALUE(ro_me) TYPE REF TO lcl_sudoku.
    METHODS constructor.
    METHODS mod_pbo_2000.
    METHODS mod_pai_2000.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES:
      mty_nr TYPE n LENGTH 1,
      BEGIN OF mty_s_sudoku,
        line    TYPE mty_nr,
        c1      TYPE mty_nr,
        c2      TYPE mty_nr,
        c3      TYPE mty_nr,
        c4      TYPE mty_nr,
        c5      TYPE mty_nr,
        c6      TYPE mty_nr,
        c7      TYPE mty_nr,
        c8      TYPE mty_nr,
        c9      TYPE mty_nr,
        t_style TYPE lvc_t_styl,
        t_color TYPE lvc_t_scol,
      END OF mty_s_sudoku,
      mty_t_sudoku TYPE STANDARD TABLE OF mty_s_sudoku WITH KEY line,
      BEGIN OF mty_s_sudoku_chk,
        line TYPE mty_nr,
        c1   TYPE mty_nr,
        c2   TYPE mty_nr,
        c3   TYPE mty_nr,
        c4   TYPE mty_nr,
        c5   TYPE mty_nr,
        c6   TYPE mty_nr,
        c7   TYPE mty_nr,
        c8   TYPE mty_nr,
        c9   TYPE mty_nr,
      END OF mty_s_sudoku_chk,
      mty_t_sudoku_chk TYPE STANDARD TABLE OF mty_s_sudoku_chk WITH KEY line.
    CONSTANTS:
      BEGIN OF mc_col1,
        col TYPE   lvc_col VALUE 4,
        int TYPE   lvc_int VALUE 0,
        inv TYPE   lvc_inv VALUE 0,
      END OF mc_col1,
      BEGIN OF mc_col2,
        col TYPE   lvc_col VALUE 2,
        int TYPE   lvc_int VALUE 0,
        inv TYPE   lvc_inv VALUE 0,
      END OF mc_col2,
      BEGIN OF mc_col3,
        col TYPE   lvc_col VALUE 3,
        int TYPE   lvc_int VALUE 0,
        inv TYPE   lvc_inv VALUE 0,
      END OF mc_col3.
    DATA mv_savegame TYPE mty_gamename.
    DATA mt_sudoku TYPE mty_t_sudoku.
    DATA mt_sudoku_solved LIKE mt_sudoku.
    DATA mv_difficulty TYPE i .
    DATA mo_alv TYPE REF TO cl_gui_alv_grid.
    DATA mo_container TYPE REF TO cl_gui_custom_container.
    DATA mt_fcat TYPE lvc_t_fcat.
    DATA mv_editable TYPE abap_bool.
    DATA mv_solved TYPE abap_bool.
    METHODS get_random_nr RETURNING VALUE(rv_nr) TYPE i.
    METHODS generate.
    METHODS create_alv.
    METHODS get_fcat RETURNING VALUE(rt_fcat) TYPE lvc_t_fcat.
    METHODS set_editable IMPORTING iv_editable TYPE abap_bool.
    METHODS get_editable RETURNING VALUE(rv_editable) TYPE abap_bool.
    METHODS hint.
    METHODS check.
    METHODS solve.
    METHODS save.
    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING er_data_changed.
ENDCLASS.

* ---------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE b1_tit.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS pa_newga TYPE abap_bool RADIOBUTTON GROUP gmod DEFAULT 'X' USER-COMMAND dummy.
SELECTION-SCREEN COMMENT 4(14) FOR FIELD pa_newga.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS pa_loadg TYPE abap_bool RADIOBUTTON GROUP gmod.
SELECTION-SCREEN COMMENT 4(14) FOR FIELD pa_loadg.
SELECTION-SCREEN COMMENT 18(1) tx_dum1 MODIF ID dum.
PARAMETERS pa_gname TYPE lcl_sudoku=>mty_gamename MODIF ID gnm.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE b2_tit.
PARAMETERS: pa_diff1 TYPE abap_bool RADIOBUTTON GROUP diff DEFAULT 'X' MODIF ID dif,
            pa_diff2 TYPE abap_bool RADIOBUTTON GROUP diff MODIF ID dif,
            pa_diff3 TYPE abap_bool RADIOBUTTON GROUP diff MODIF ID dif.
SELECTION-SCREEN END OF BLOCK b2.

* ---------------------------------------------------------------------
DATA:
  gv_ucomm  TYPE sy-ucomm,
  go_sudoku TYPE REF TO lcl_sudoku.

* ---------------------------------------------------------------------
INITIALIZATION.
  b1_tit    = 'Mode'(002).
  b2_tit    = 'Select Difficulty'(001).

* ---------------------------------------------------------------------
AT SELECTION-SCREEN ON pa_gname.
  IF sy-ucomm <> 'DUMMY'
  AND pa_loadg = abap_true.
    lcl_sudoku=>chck_gamename( pa_gname ).
  ENDIF.

* ---------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_gname.
  pa_gname = lcl_sudoku=>shlp_gamename( ).

* ---------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'GNM'.
        CASE abap_true.
          WHEN pa_newga.
            screen-invisible = 1.
            screen-input     = 0.
          WHEN pa_loadg.
            screen-invisible = 0.
            screen-input     = 1.
        ENDCASE.
        MODIFY SCREEN.
      WHEN 'DIF'.
        CASE abap_true.
          WHEN pa_newga.
            screen-invisible = 0.
            screen-input     = 1.
          WHEN pa_loadg.
            screen-invisible = 1.
            screen-input     = 0.
        ENDCASE.
        MODIFY SCREEN.
      WHEN 'DUM'.
        screen-active = 0.
        MODIFY SCREEN.
      WHEN OTHERS.
    ENDCASE.
  ENDLOOP.

* ---------------------------------------------------------------------
START-OF-SELECTION.
  IF ( pa_loadg = abap_true
       AND lcl_sudoku=>chck_gamename( pa_gname ) = abap_true ).
    go_sudoku = lcl_sudoku=>load_savegame( pa_gname ).
    IF go_sudoku IS BOUND.
      CALL SELECTION-SCREEN 2000.
    ENDIF.
  ELSEIF pa_newga = abap_true.
    go_sudoku = NEW #( ).
    CALL SELECTION-SCREEN 2000.
  ENDIF.

* --------------------------------------------------------------------------------------------------
MODULE status_2000 OUTPUT.
  go_sudoku->mod_pbo_2000( ).
ENDMODULE.

* --------------------------------------------------------------------------------------------------
MODULE user_command_2000 INPUT.
  go_sudoku->mod_pai_2000( ).
ENDMODULE.

* --------------------------------------------------------------------------------------------------
CLASS lcl_sudoku IMPLEMENTATION.

  METHOD load_savegame.
* --------------------------------------------------------------------------------------------------
    DATA:
      ls_savegame TYPE zwd_sudoku_saveg.

* --------------------------------------------------------------------------------------------------
    SELECT SINGLE * FROM zwd_sudoku_saveg INTO ls_savegame WHERE username = sy-uname AND savegame = pa_gname.
    CALL TRANSFORMATION id SOURCE XML ls_savegame-data RESULT object = ro_me.

* --------------------------------------------------------------------------------------------------
    IF ro_me IS BOUND.
      ro_me->create_alv( ).
      ro_me->set_editable( abap_false ).
    ELSE.
      MESSAGE 'Loading failed!' TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD shlp_gamename.
* --------------------------------------------------------------------------------------------------
    DATA:
      lt_games  TYPE STANDARD TABLE OF zwd_sudoku_saveg WITH DEFAULT KEY,
      lv_answer TYPE c LENGTH 3,
      lt_list   TYPE STANDARD TABLE OF spopli WITH DEFAULT KEY.

* --------------------------------------------------------------------------------------------------
    SELECT savegame
    FROM zwd_sudoku_saveg
    INTO CORRESPONDING FIELDS OF TABLE lt_games
    WHERE username = sy-uname.

* --------------------------------------------------------------------------------------------------
    LOOP AT lt_games ASSIGNING FIELD-SYMBOL(<ls_game>).
      APPEND VALUE #( varoption = <ls_game>-savegame ) TO lt_list.
    ENDLOOP.

* --------------------------------------------------------------------------------------------------
    IF lt_list IS NOT INITIAL.
      CALL FUNCTION 'POPUP_TO_DECIDE_LIST'
        EXPORTING
          textline1 = 'Choose Saved Game for'(004)
          textline2 = sy-uname
          titel     = 'Choose Saved Game'(003)
        IMPORTING
          answer    = lv_answer
        TABLES
          t_spopli  = lt_list
        EXCEPTIONS
          OTHERS    = 0.
    ELSE.
      MESSAGE 'No saved games available!' TYPE 'W'.
    ENDIF.

* --------------------------------------------------------------------------------------------------
    IF lv_answer IS NOT INITIAL
    AND lv_answer <> 'A'.
      READ TABLE lt_list ASSIGNING FIELD-SYMBOL(<ls_list>) INDEX lv_answer.
      IF <ls_list> IS ASSIGNED.
        rv_gamename = <ls_list>-varoption.
      ENDIF.
    ELSEIF lv_answer = 'A'.
      MESSAGE 'Canceled' TYPE 'S' DISPLAY LIKE 'W'.
    ENDIF.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD chck_gamename.
* --------------------------------------------------------------------------------------------------
    IF iv_gamename IS INITIAL.
      MESSAGE 'Enter Savgame!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

* --------------------------------------------------------------------------------------------------
    SELECT COUNT(*) FROM zwd_sudoku_saveg WHERE username = sy-uname AND savegame = iv_gamename.
    IF sy-subrc <> 0.
      rv_exists = abap_false.
      MESSAGE `Savegame ` && iv_gamename && ` does not exist!` TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      rv_exists = abap_true.
    ENDIF.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.


  METHOD save.
* --------------------------------------------------------------------------------------------------
    DATA:
      ls_savegame TYPE zwd_sudoku_saveg,
      lt_fields   TYPE STANDARD TABLE OF sval WITH DEFAULT KEY,
      lv_return   TYPE flag,
      lv_answer   TYPE flag.

* --------------------------------------------------------------------------------------------------
    APPEND VALUE #( tabname = 'ZWD_SUDOKU_SAVEG' fieldname = 'USERNAME' value = sy-uname field_attr = '02' ) TO lt_fields.
    APPEND VALUE #( tabname = 'ZWD_SUDOKU_SAVEG' fieldname = 'SAVEGAME' value = mv_savegame ) TO lt_fields.

    DO 100 TIMES.

* --------------------------------------------------------------------------------------------------
      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          no_value_check = abap_true
          popup_title    = 'Save Game'(005)
        IMPORTING
          returncode     = lv_return
        TABLES
          fields         = lt_fields
        EXCEPTIONS
          OTHERS         = 0.

      IF lv_return <> 'A'.
        READ TABLE lt_fields ASSIGNING FIELD-SYMBOL(<ls_field>) WITH KEY fieldname = 'SAVEGAME'.
        IF <ls_field> IS ASSIGNED.
          ls_savegame-username = sy-uname.
          ls_savegame-savegame = <ls_field>-value.
          SELECT COUNT(*) FROM zwd_sudoku_saveg WHERE username = ls_savegame-username AND savegame = ls_savegame-savegame.
          IF sy-subrc = 0.
            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                text_question         = 'Game already exists! Override?'(006)
                display_cancel_button = abap_false
                popup_type            = 'ICON_MESSAGE_WARNING'
              IMPORTING
                answer                = lv_answer
              EXCEPTIONS
                OTHERS                = 0.
            CASE lv_answer.
              WHEN '1'.
                DELETE FROM zwd_sudoku_saveg WHERE username = ls_savegame-username AND savegame = ls_savegame-savegame.
              WHEN OTHERS.
                CONTINUE.
            ENDCASE.
          ENDIF.
          mv_savegame = ls_savegame-savegame.
          CALL TRANSFORMATION id SOURCE object = me RESULT XML ls_savegame-data.
          INSERT zwd_sudoku_saveg FROM ls_savegame.
          MESSAGE 'Game saved!' TYPE 'S'.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE 'Canceled' TYPE 'W'.
        EXIT.
      ENDIF.
    ENDDO.

* --------------------------------------------------------------------------------------------------
    COMMIT WORK.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD hint.
* --------------------------------------------------------------------------------------------------
    DATA:
      lv_x TYPE i,
      lv_y TYPE i.

* --------------------------------------------------------------------------------------------------
    DO 100 TIMES.
      lv_x = get_random_nr( ).
      lv_y = get_random_nr( ).
      READ TABLE mt_sudoku ASSIGNING FIELD-SYMBOL(<ls_sudoku>) INDEX lv_x.
      IF <ls_sudoku> IS ASSIGNED.
        ASSIGN COMPONENT lv_y + 1 OF STRUCTURE <ls_sudoku> TO FIELD-SYMBOL(<lv_nr>).
        IF  <lv_nr> IS ASSIGNED
        AND <lv_nr> IS INITIAL.
          READ TABLE mt_sudoku_solved ASSIGNING FIELD-SYMBOL(<ls_sudoku_solved>) INDEX lv_x.
          IF <ls_sudoku> IS ASSIGNED.
            ASSIGN COMPONENT lv_y + 1 OF STRUCTURE <ls_sudoku_solved> TO FIELD-SYMBOL(<lv_nr_solved>).
            IF <lv_nr_solved> IS ASSIGNED.
              <lv_nr> = <lv_nr_solved>.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDDO.
    mo_alv->refresh_table_display( ).

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD check.
* --------------------------------------------------------------------------------------------------
    DATA:
      lt_sudoku        TYPE mty_t_sudoku_chk,
      lt_sudoku_solved TYPE mty_t_sudoku_chk,
      lv_x             TYPE i,
      lv_y             TYPE i,
      lv_colf          TYPE fieldname,
      lv_colf_solved   TYPE fieldname,
      lv_coln          TYPE fieldname.

* --------------------------------------------------------------------------------------------------
    MOVE-CORRESPONDING mt_sudoku        TO lt_sudoku.
    MOVE-CORRESPONDING mt_sudoku_solved TO lt_sudoku_solved.
    IF lt_sudoku = lt_sudoku_solved.
      MESSAGE 'You did it!' TYPE 'S'.
      mt_sudoku = mt_sudoku_solved.
      set_editable( abap_false ).
      mv_solved = abap_true.
      RETURN.
    ENDIF.

* --------------------------------------------------------------------------------------------------
    LOOP AT mt_sudoku ASSIGNING FIELD-SYMBOL(<ls_sudoku>).
      lv_x = sy-tabix.
      DO 9 TIMES.
        lv_y = sy-index.
        lv_coln        = 'C' && lv_y.
        lv_colf        = '<LS_SUDOKU>-'        && lv_coln.
        lv_colf_solved = '<LS_SUDOKU_SOLVED>-' && lv_coln.
        READ TABLE <ls_sudoku>-t_style TRANSPORTING NO FIELDS WITH KEY fieldname = lv_coln BINARY SEARCH.
        IF sy-subrc <> 0.
          ASSIGN (lv_colf) TO FIELD-SYMBOL(<lv_nr>).
          IF  <lv_nr> IS ASSIGNED
          AND <lv_nr> IS NOT INITIAL.
            READ TABLE mt_sudoku_solved ASSIGNING FIELD-SYMBOL(<ls_sudoku_solved>) INDEX lv_x.
            IF <ls_sudoku_solved> IS ASSIGNED.
              ASSIGN (lv_colf_solved) TO FIELD-SYMBOL(<lv_nr_solved>).
              IF <lv_nr_solved> IS ASSIGNED.
                IF <lv_nr> = <lv_nr_solved>.
                  DELETE <ls_sudoku>-t_color WHERE fname = lv_coln.
                  APPEND VALUE #( fname = lv_coln color = mc_col3 ) TO <ls_sudoku>-t_color.
                  READ TABLE <ls_sudoku_solved>-t_style ASSIGNING FIELD-SYMBOL(<ls_style_solved>) WITH KEY fieldname = lv_coln BINARY SEARCH.
                  IF <ls_style_solved> IS ASSIGNED.
                    INSERT <ls_style_solved> INTO TABLE <ls_sudoku>-t_style.
                  ENDIF.
                  CONTINUE.
                ELSE.
                  mo_alv->refresh_table_display( ).
                  MESSAGE 'Incorrect' TYPE 'S' DISPLAY LIKE 'W'.
                  RETURN.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDDO.
    ENDLOOP.

* --------------------------------------------------------------------------------------------------
    mo_alv->refresh_table_display( ).
    MESSAGE 'OK!' TYPE 'S'.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.


  METHOD solve.
* --------------------------------------------------------------------------------------------------
    mt_sudoku = mt_sudoku_solved.
    mo_alv->refresh_table_display( ).

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD mod_pai_2000.
* --------------------------------------------------------------------------------------------------
    CASE gv_ucomm.
      WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
        LEAVE TO SCREEN 0.
      WHEN 'START'.
        set_editable( abap_true ).
      WHEN 'CHECK'.
        check( ).
      WHEN 'HINT'.
        hint( ).
      WHEN 'SOLVE'.
        solve( ).
      WHEN 'SAVE'.
        save( ).
    ENDCASE.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD constructor.
* --------------------------------------------------------------------------------------------------
    CASE abap_true.
      WHEN pa_diff1.
        mv_difficulty = lcl_sudoku=>difficulty_easy.
      WHEN pa_diff2.
        mv_difficulty = lcl_sudoku=>difficulty_medium.
      WHEN pa_diff3.
        mv_difficulty = lcl_sudoku=>difficulty_hard.
      WHEN OTHERS.
        mv_difficulty = lcl_sudoku=>difficulty_easy.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
    INSERT VALUE #( line = 1 ) INTO TABLE mt_sudoku.
    INSERT VALUE #( line = 2 ) INTO TABLE mt_sudoku.
    INSERT VALUE #( line = 3 ) INTO TABLE mt_sudoku.
    INSERT VALUE #( line = 4 ) INTO TABLE mt_sudoku.
    INSERT VALUE #( line = 5 ) INTO TABLE mt_sudoku.
    INSERT VALUE #( line = 6 ) INTO TABLE mt_sudoku.
    INSERT VALUE #( line = 7 ) INTO TABLE mt_sudoku.
    INSERT VALUE #( line = 8 ) INTO TABLE mt_sudoku.
    INSERT VALUE #( line = 9 ) INTO TABLE mt_sudoku.

* --------------------------------------------------------------------------------------------------
    generate( ).
    create_alv( ).

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.


  METHOD create_alv.
* --------------------------------------------------------------------------------------------------
    DATA:
      lt_fcat   TYPE lvc_t_fcat,
      ls_layout TYPE lvc_s_layo.

* --------------------------------------------------------------------------------------------------
    mo_container = NEW #( container_name = 'CONTROL1' ).
    mo_alv = NEW #( i_parent = mo_container ).

* --------------------------------------------------------------------------------------------------
    ls_layout-stylefname = 'T_STYLE'.
    ls_layout-ctab_fname = 'T_COLOR'.
    ls_layout-edit       = abap_true.
    ls_layout-no_toolbar = abap_true.
    ls_layout-no_rowmove = abap_true.
    ls_layout-no_rowins  = abap_true.
    ls_layout-no_colexpd = abap_true.
    ls_layout-no_headers = abap_true.
    ls_layout-no_rowmark = abap_true.
    ls_layout-sel_mode   = 'A'.

* --------------------------------------------------------------------------------------------------
    mo_alv->register_edit_event( mo_alv->mc_evt_modified ).
    SET HANDLER handle_data_changed FOR mo_alv.

* --------------------------------------------------------------------------------------------------
    lt_fcat = get_fcat( ).

* --------------------------------------------------------------------------------------------------
    mo_alv->set_table_for_first_display( EXPORTING  is_layout       = ls_layout
                                         CHANGING   it_outtab       = mt_sudoku
                                                    it_fieldcatalog = lt_fcat
                                         EXCEPTIONS OTHERS          = 4         ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* --------------------------------------------------------------------------------------------------
    mo_alv->set_ready_for_input( i_ready_for_input = 0 ).

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD get_fcat.
* --------------------------------------------------------------------------------------------------
    DATA:
      lo_salv_table TYPE REF TO cl_salv_table.

* --------------------------------------------------------------------------------------------------
    IF mt_fcat IS NOT INITIAL.
      rt_fcat = mt_fcat.
    ENDIF.

* --------------------------------------------------------------------------------------------------
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_salv_table
                                CHANGING  t_table      = mt_sudoku     ).
        mt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog( r_columns      = lo_salv_table->get_columns( )
                                                                     r_aggregations = lo_salv_table->get_aggregations( ) ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

* --------------------------------------------------------------------------------------------------
    LOOP AT mt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
      CASE <ls_fcat>-fieldname.
        WHEN 'LINE'.
          <ls_fcat>-tech = abap_true.
        WHEN OTHERS.
          <ls_fcat>-fix_column = abap_true.
          <ls_fcat>-outputlen = 2.
          <ls_fcat>-edit = abap_true.
      ENDCASE.
    ENDLOOP.

* --------------------------------------------------------------------------------------------------
    rt_fcat = mt_fcat.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD generate.
* --------------------------------------------------------------------------------------------------
    CONSTANTS:
      lc_max_do TYPE i VALUE 1000.
    DATA:
      lv_row1    TYPE string,
      lv_row2    TYPE string,
      lv_row3    TYPE string,
      lv_row4    TYPE string,
      lv_row5    TYPE string,
      lv_row6    TYPE string,
      lv_row7    TYPE string,
      lv_row8    TYPE string,
      lv_row9    TYPE string,
      lv_col1    TYPE string,
      lv_col2    TYPE string,
      lv_col3    TYPE string,
      lv_col4    TYPE string,
      lv_col5    TYPE string,
      lv_col6    TYPE string,
      lv_col7    TYPE string,
      lv_col8    TYPE string,
      lv_col9    TYPE string,
      lv_blk1    TYPE string,
      lv_blk2    TYPE string,
      lv_blk3    TYPE string,
      lv_blk4    TYPE string,
      lv_blk5    TYPE string,
      lv_blk6    TYPE string,
      lv_blk7    TYPE string,
      lv_blk8    TYPE string,
      lv_blk9    TYPE string,
      lv_tabix   TYPE sy-tabix,
      lv_row     TYPE fieldname,
      lv_add     TYPE i,
      lv_blk_nr  TYPE i,
      lv_blk     TYPE fieldname,
      lv_do1     TYPE i,
      lv_do2     TYPE i,
      lv_do3     TYPE i,
      lv_do4     TYPE i,
      lv_do5     TYPE i,
      lv_do6     TYPE i,
      lv_do7     TYPE i,
      lv_do8     TYPE i,
      lv_do9     TYPE i,
      lv_restart TYPE abap_bool.
    FIELD-SYMBOLS:
      <lv_row> TYPE string,
      <lv_blk> TYPE string.

* --------------------------------------------------------------------------------------------------
    mt_sudoku_solved = mt_sudoku.

* --------------------------------------------------------------------------------------------------
    DO 10000 TIMES.
      lv_restart = abap_false.
      FREE: lv_row1, lv_row2, lv_row3, lv_row4, lv_row5, lv_row6, lv_row7, lv_row8, lv_row9,
            lv_col1, lv_col2, lv_col3, lv_col4, lv_col5, lv_col6, lv_col7, lv_col8, lv_col9,
            lv_blk1, lv_blk2, lv_blk3, lv_blk4, lv_blk5, lv_blk6, lv_blk7, lv_blk8, lv_blk9.

      LOOP AT mt_sudoku_solved ASSIGNING FIELD-SYMBOL(<ls_sudoku>).
        FREE: lv_do1, lv_do2, lv_do3, lv_do4, lv_do5, lv_do6, lv_do7, lv_do8, lv_do9,
              <ls_sudoku>-c1, <ls_sudoku>-c1, <ls_sudoku>-c1, <ls_sudoku>-c1, <ls_sudoku>-c1, <ls_sudoku>-c1, <ls_sudoku>-c1, <ls_sudoku>-c1, <ls_sudoku>-c1,
              <ls_sudoku>-t_color, <ls_sudoku>-t_style.
        lv_tabix = sy-tabix.
        lv_row = 'LV_ROW' && lv_tabix.
        ASSIGN (lv_row) TO <lv_row>.
        CASE lv_tabix.
          WHEN 1 OR 2 OR 3.
            lv_add = 0.
          WHEN 4 OR 5 OR 6.
            lv_add = 3.
          WHEN 7 OR 8 OR 9.
            lv_add = 6.
        ENDCASE.
        lv_blk_nr = lv_add + 1.
        lv_blk = 'LV_BLK' && lv_blk_nr.
        ASSIGN (lv_blk) TO <lv_blk>.
        IF lv_blk_nr MOD 2 = 0.
          INSERT VALUE #( fname = 'C1' color = mc_col1 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C2' color = mc_col1 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C3' color = mc_col1 ) INTO TABLE <ls_sudoku>-t_color.
        ELSE.
          INSERT VALUE #( fname = 'C1' color = mc_col2 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C2' color = mc_col2 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C3' color = mc_col2 ) INTO TABLE <ls_sudoku>-t_color.
        ENDIF.
        DO lc_max_do TIMES.
          <ls_sudoku>-c1 = get_random_nr( ).
          IF  <ls_sudoku>-c1 NA <lv_row>
          AND <ls_sudoku>-c1 NA <lv_blk>
          AND <ls_sudoku>-c1 NA lv_col1.
            <lv_row> = <lv_row> && <ls_sudoku>-c1.
            <lv_blk> = <lv_blk> && <ls_sudoku>-c1.
            lv_col1 = lv_col1 && <ls_sudoku>-c1.
            EXIT.
          ENDIF.
          lv_do1 = lv_do1 + 1.
        ENDDO.
        IF lv_do1 = lc_max_do.
          lv_restart = abap_true.
          EXIT.
        ENDIF.
        DO lc_max_do TIMES.
          <ls_sudoku>-c2 = get_random_nr( ).
          IF  <ls_sudoku>-c2 NA <lv_row>
          AND <ls_sudoku>-c2 NA <lv_blk>
          AND <ls_sudoku>-c2 NA lv_col2.
            <lv_row> = <lv_row> && <ls_sudoku>-c2.
            <lv_blk> = <lv_blk> && <ls_sudoku>-c2.
            lv_col2 = lv_col2 && <ls_sudoku>-c2.
            EXIT.
          ENDIF.
          lv_do2 = lv_do2 + 1.
        ENDDO.
        IF lv_do2 = lc_max_do.
          lv_restart = abap_true.
          EXIT.
        ENDIF.
        DO lc_max_do TIMES.
          <ls_sudoku>-c3 = get_random_nr( ).
          IF  <ls_sudoku>-c3 NA <lv_row>
          AND <ls_sudoku>-c3 NA <lv_blk>
          AND <ls_sudoku>-c3 NA lv_col3.
            <lv_row> = <lv_row> && <ls_sudoku>-c3.
            <lv_blk> = <lv_blk> && <ls_sudoku>-c3.
            lv_col3 = lv_col3 && <ls_sudoku>-c3.
            EXIT.
          ENDIF.
          lv_do3 = lv_do3 + 1.
        ENDDO.
        IF lv_do3 = lc_max_do.
          lv_restart = abap_true.
          EXIT.
        ENDIF.
        lv_blk_nr = lv_add + 2.
        lv_blk = 'LV_BLK' && lv_blk_nr.
        ASSIGN (lv_blk) TO <lv_blk>.
        IF lv_blk_nr MOD 2 = 0.
          INSERT VALUE #( fname = 'C4' color = mc_col1 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C5' color = mc_col1 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C6' color = mc_col1 ) INTO TABLE <ls_sudoku>-t_color.
        ELSE.
          INSERT VALUE #( fname = 'C4' color = mc_col2 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C5' color = mc_col2 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C6' color = mc_col2 ) INTO TABLE <ls_sudoku>-t_color.
        ENDIF.
        DO lc_max_do TIMES.
          <ls_sudoku>-c4 = get_random_nr( ).
          IF  <ls_sudoku>-c4 NA <lv_row>
          AND <ls_sudoku>-c4 NA <lv_blk>
          AND <ls_sudoku>-c4 NA lv_col4.
            <lv_row> = <lv_row> && <ls_sudoku>-c4.
            <lv_blk> = <lv_blk> && <ls_sudoku>-c4.
            lv_col4 = lv_col4 && <ls_sudoku>-c4.
            EXIT.
          ENDIF.
          lv_do4 = lv_do4 + 1.
        ENDDO.
        IF lv_do4 = lc_max_do.
          lv_restart = abap_true.
          EXIT.
        ENDIF.
        DO lc_max_do TIMES.
          <ls_sudoku>-c5 = get_random_nr( ).
          IF  <ls_sudoku>-c5 NA <lv_row>
          AND <ls_sudoku>-c5 NA <lv_blk>
          AND <ls_sudoku>-c5 NA lv_col5.
            <lv_row> = <lv_row> && <ls_sudoku>-c5.
            <lv_blk> = <lv_blk> && <ls_sudoku>-c5.
            lv_col5 = lv_col5 && <ls_sudoku>-c5.
            EXIT.
          ENDIF.
          lv_do5 = lv_do5 + 1.
        ENDDO.
        IF lv_do5 = lc_max_do.
          lv_restart = abap_true.
          EXIT.
        ENDIF.
        DO lc_max_do TIMES.
          <ls_sudoku>-c6 = get_random_nr( ).
          IF  <ls_sudoku>-c6 NA <lv_row>
          AND <ls_sudoku>-c6 NA <lv_blk>
          AND <ls_sudoku>-c6 NA lv_col6.
            <lv_row> = <lv_row> && <ls_sudoku>-c6.
            <lv_blk> = <lv_blk> && <ls_sudoku>-c6.
            lv_col6 = lv_col6 && <ls_sudoku>-c6.
            EXIT.
          ENDIF.
          lv_do6 = lv_do6 + 1.
        ENDDO.
        IF lv_do6 = lc_max_do.
          lv_restart = abap_true.
          EXIT.
        ENDIF.
        lv_blk_nr = lv_add + 3.
        lv_blk = 'LV_BLK' && lv_blk_nr.
        ASSIGN (lv_blk) TO <lv_blk>.
        IF lv_blk_nr MOD 2 = 0.
          INSERT VALUE #( fname = 'C7' color = mc_col1 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C8' color = mc_col1 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C9' color = mc_col1 ) INTO TABLE <ls_sudoku>-t_color.
        ELSE.
          INSERT VALUE #( fname = 'C7' color = mc_col2 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C8' color = mc_col2 ) INTO TABLE <ls_sudoku>-t_color.
          INSERT VALUE #( fname = 'C9' color = mc_col2 ) INTO TABLE <ls_sudoku>-t_color.
        ENDIF.
        DO lc_max_do TIMES.
          <ls_sudoku>-c7 = get_random_nr( ).
          IF  <ls_sudoku>-c7 NA <lv_row>
          AND <ls_sudoku>-c7 NA <lv_blk>
          AND <ls_sudoku>-c7 NA lv_col7.
            <lv_row> = <lv_row> && <ls_sudoku>-c7.
            <lv_blk> = <lv_blk> && <ls_sudoku>-c7.
            lv_col7 = lv_col7 && <ls_sudoku>-c7.
            EXIT.
          ENDIF.
          lv_do7 = lv_do7 + 1.
        ENDDO.
        IF lv_do7 = lc_max_do.
          lv_restart = abap_true.
          EXIT.
        ENDIF.
        DO lc_max_do TIMES.
          <ls_sudoku>-c8 = get_random_nr( ).
          IF  <ls_sudoku>-c8 NA <lv_row>
          AND <ls_sudoku>-c8 NA <lv_blk>
          AND <ls_sudoku>-c8 NA lv_col8.
            <lv_row> = <lv_row> && <ls_sudoku>-c8.
            <lv_blk> = <lv_blk> && <ls_sudoku>-c8.
            lv_col8 = lv_col8 && <ls_sudoku>-c8.
            EXIT.
          ENDIF.
          lv_do8 = lv_do8 + 1.
        ENDDO.
        IF lv_do8 = lc_max_do.
          lv_restart = abap_true.
          EXIT.
        ENDIF.
        DO lc_max_do TIMES.
          <ls_sudoku>-c9 = get_random_nr( ).
          IF  <ls_sudoku>-c9 NA <lv_row>
          AND <ls_sudoku>-c9 NA <lv_blk>
          AND <ls_sudoku>-c9 NA lv_col9.
            <lv_row> = <lv_row> && <ls_sudoku>-c9.
            <lv_blk> = <lv_blk> && <ls_sudoku>-c9.
            lv_col9 = lv_col9 && <ls_sudoku>-c9.
            EXIT.
          ENDIF.
          lv_do9 = lv_do9 + 1.
        ENDDO.
        IF lv_do9 = lc_max_do.
          lv_restart = abap_true.
          EXIT.
        ENDIF.
        INSERT VALUE #( fieldname = 'C1' style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE <ls_sudoku>-t_style.
        INSERT VALUE #( fieldname = 'C2' style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE <ls_sudoku>-t_style.
        INSERT VALUE #( fieldname = 'C3' style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE <ls_sudoku>-t_style.
        INSERT VALUE #( fieldname = 'C4' style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE <ls_sudoku>-t_style.
        INSERT VALUE #( fieldname = 'C5' style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE <ls_sudoku>-t_style.
        INSERT VALUE #( fieldname = 'C6' style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE <ls_sudoku>-t_style.
        INSERT VALUE #( fieldname = 'C7' style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE <ls_sudoku>-t_style.
        INSERT VALUE #( fieldname = 'C8' style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE <ls_sudoku>-t_style.
        INSERT VALUE #( fieldname = 'C9' style = cl_gui_alv_grid=>mc_style_disabled ) INTO TABLE <ls_sudoku>-t_style.
      ENDLOOP.
      IF lv_restart = abap_false.
        EXIT.
      ENDIF.
    ENDDO.

* --------------------------------------------------------------------------------------------------
    mt_sudoku = mt_sudoku_solved.

* --------------------------------------------------------------------------------------------------
    DATA:
      lv_del_max TYPE i,
      lv_del     TYPE i,
      lv_x       TYPE i,
      lv_y       TYPE i,
      lv_colf    TYPE fieldname,
      lv_coln    TYPE fieldname.
    FIELD-SYMBOLS:
    <lv_col> TYPE mty_nr.

* --------------------------------------------------------------------------------------------------
    CASE mv_difficulty.
      WHEN difficulty_easy.
        lv_del_max = 22.
      WHEN difficulty_medium.
        lv_del_max = 26.
      WHEN difficulty_hard.
        lv_del_max = 30.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
    DO 100 TIMES.
      lv_x = get_random_nr( ).
      lv_y = get_random_nr( ).
      lv_coln = 'C' && lv_y.
      lv_colf = '<LS_SUDOKU>-' && lv_coln.
      UNASSIGN: <ls_sudoku>, <lv_col>.
      READ TABLE mt_sudoku ASSIGNING <ls_sudoku> INDEX lv_x.
      IF <ls_sudoku> IS ASSIGNED.
        ASSIGN (lv_colf) TO <lv_col>.
        IF  <lv_col> IS ASSIGNED
        AND <lv_col> IS NOT INITIAL.
          FREE <lv_col>.
          lv_del = lv_del + 1.
        ENDIF.
      ENDIF.
      IF lv_del = lv_del_max.
        EXIT.
      ENDIF.
    ENDDO.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_random_nr.
* --------------------------------------------------------------------------------------------------
    " return random number between 1 and 9.
    " logic partly stolen from function module GENERAL_GET_RANDOM_INT

* --------------------------------------------------------------------------------------------------
    DATA:
    lv_fraction TYPE f.

* --------------------------------------------------------------------------------------------------
    DO 10 TIMES.
      CALL 'RSEC_GEN_RAND' ID 'OUTPUT'     FIELD rv_nr
                           ID 'FORCE_INIT' FIELD ' '.    "#EC CI_CCALL.

* --------------------------------------------------------------------------------------------------
      IF sy-subrc = 0.
        " We got a random number in the range 0 to 2^31-1. Divide by the latter to get a normalized random number.
        lv_fraction = rv_nr / 2147483647.
        " Multiply with max allowed value to get a value within range and round
        rv_nr = 10 * lv_fraction - '+0.5'.
        IF rv_nr <> 0.
          EXIT.
        ENDIF.
      ENDIF.
    ENDDO.

* --------------------------------------------------------------------------------------------------
    IF rv_nr = 0.
      " number does not actually need to be perfectly random for the purpose of this method
      DO 10000 TIMES.
        GET TIME.
        rv_nr = sy-uzeit+5(1).
        IF rv_nr <> 0.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD handle_data_changed.
* --------------------------------------------------------------------------------------------------
    er_data_changed->refresh_protocol( ).
    LOOP AT er_data_changed->mt_mod_cells ASSIGNING FIELD-SYMBOL(<ls_mod_cell>).
      IF  <ls_mod_cell>-value <> space
      AND <ls_mod_cell>-value NA '0123456789'.
        READ TABLE mt_sudoku ASSIGNING FIELD-SYMBOL(<ls_sudoku>) INDEX <ls_mod_cell>-row_id.
        ASSIGN COMPONENT <ls_mod_cell>-fieldname OF STRUCTURE <ls_sudoku> TO FIELD-SYMBOL(<ls_nr>).
        er_data_changed->modify_cell( i_row_id    = <ls_mod_cell>-row_id
                                      i_fieldname = <ls_mod_cell>-fieldname
                                      i_value     = <ls_nr>                 ).
      ENDIF.
    ENDLOOP.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.


  METHOD mod_pbo_2000.
* --------------------------------------------------------------------------------------------------
    DATA:
    lt_excl TYPE TABLE OF sy-ucomm.

* --------------------------------------------------------------------------------------------------
    CASE mv_solved.
      WHEN abap_true.
        APPEND 'START' TO lt_excl.
        APPEND 'CHECK' TO lt_excl.
        APPEND 'HINT' TO lt_excl.
        APPEND 'SOLVE' TO lt_excl.
      WHEN OTHERS.
        CASE get_editable( ).
          WHEN abap_true.
            APPEND 'START' TO lt_excl.
          WHEN abap_false.
            APPEND 'CHECK' TO lt_excl.
            APPEND 'HINT' TO lt_excl.
            APPEND 'SOLVE' TO lt_excl.
        ENDCASE.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
    SET PF-STATUS 'STATUS_2000' EXCLUDING lt_excl.
    SET TITLEBAR 'TITLE_2000'.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_editable.
* --------------------------------------------------------------------------------------------------
    DATA:
      lt_fnam TYPE STANDARD TABLE OF fieldname,
      lv_coln TYPE fieldname.

* --------------------------------------------------------------------------------------------------
    CASE iv_editable.
      WHEN abap_true.
        mo_alv->set_ready_for_input( i_ready_for_input = 1 ).
        LOOP AT mt_sudoku ASSIGNING FIELD-SYMBOL(<ls_sudoku>).
          FREE lt_fnam.
          DO 10 TIMES.
            DATA(lv_index) = sy-index + 1.
            ASSIGN COMPONENT lv_index OF STRUCTURE <ls_sudoku> TO FIELD-SYMBOL(<lv_nr>).
            IF  <lv_nr> IS ASSIGNED
            AND <lv_nr> IS INITIAL.
              lv_index = lv_index - 1.
              APPEND 'C' && lv_index TO lt_fnam.
            ENDIF.
          ENDDO.
          LOOP AT lt_fnam ASSIGNING FIELD-SYMBOL(<lv_fnam>).
            DELETE <ls_sudoku>-t_style WHERE fieldname = <lv_fnam>.
            DELETE <ls_sudoku>-t_color WHERE fname     = <lv_fnam>.
          ENDLOOP.
        ENDLOOP.
        mo_alv->refresh_table_display( ).
        mv_editable = abap_true.
      WHEN OTHERS.
        mo_alv->set_ready_for_input( i_ready_for_input = 0 ).
        mo_alv->refresh_table_display( ).
        mv_editable = abap_false.
    ENDCASE.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.

  METHOD get_editable.
* --------------------------------------------------------------------------------------------------
    rv_editable = mv_editable.

* --------------------------------------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
