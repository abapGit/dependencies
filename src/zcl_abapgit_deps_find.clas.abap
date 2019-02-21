CLASS zcl_abapgit_deps_find DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_package TYPE devclass .
    METHODS find
      RETURNING
        VALUE(rt_tadir) TYPE zif_abapgit_definitions=>ty_tadir_tt
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    DATA mt_total TYPE if_ris_environment_types=>ty_t_senvi_tadir .
    DATA mv_max_level TYPE i VALUE 20 ##NO_TEXT.
    DATA mv_package TYPE devclass .

    METHODS find_clas_dependencies
      IMPORTING
        !iv_name  TYPE tadir-obj_name
        !iv_level TYPE i
      CHANGING
        !ct_tadir TYPE if_ris_environment_types=>ty_t_senvi_tadir .
    METHODS get_dependencies
      IMPORTING
        !is_object TYPE zif_abapgit_definitions=>ty_tadir
        !iv_level  TYPE i .
    METHODS resolve
      IMPORTING
        !it_wbcrossgt TYPE wbcrossgtt
      CHANGING
        !ct_tadir     TYPE if_ris_environment_types=>ty_t_senvi_tadir .
    METHODS update_index
      IMPORTING
        !iv_name TYPE seoclsname .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_DEPS_FIND IMPLEMENTATION.


  METHOD constructor.

    ASSERT NOT iv_package IS INITIAL.

    mv_package = iv_package.

  ENDMETHOD.


  METHOD find.

    DATA(lt_objects) = zcl_abapgit_factory=>get_tadir( )->read( mv_package ).
    DELETE lt_objects WHERE object = 'DEVC'.
    DELETE lt_objects WHERE object = 'TRAN'. " todo, hmm?

* todo, skip generated maintenance view function groups?

    LOOP AT lt_objects INTO DATA(ls_object).
      cl_progress_indicator=>progress_indicate(
        i_text               = |Finding dependencies, { ls_object-object } { ls_object-obj_name }|
        i_processed          = sy-tabix
        i_total              = lines( lt_objects )
        i_output_immediately = abap_true ).

      get_dependencies(
        is_object = ls_object
        iv_level  = 1 ).
    ENDLOOP.

    SORT mt_total BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM mt_total COMPARING ref_obj_type ref_obj_name.

    LOOP AT mt_total INTO DATA(ls_total).
      WRITE: / ls_total-ref_obj_type, ls_total-ref_obj_name.

      APPEND VALUE #(
        object   = ls_total-ref_obj_type
        obj_name = ls_total-ref_obj_name ) TO rt_tadir.
    ENDLOOP.

  ENDMETHOD.


  METHOD find_clas_dependencies.

* todo, type of CT_TADIR ? huh

    DATA: lt_includes TYPE STANDARD TABLE OF programm WITH EMPTY KEY.

    DATA(lv_clsname) = CONV seoclsname( iv_name ).

    TRY.
        DATA(lv_final) = CAST cl_oo_class( cl_oo_class=>get_instance( lv_clsname ) )->is_final( ).
      CATCH cx_class_not_existent.
        RETURN.
    ENDTRY.

    APPEND cl_oo_classname_service=>get_pubsec_name( CONV #( iv_name ) ) TO lt_includes.
    IF lv_final = abap_false.
      APPEND cl_oo_classname_service=>get_prisec_name( CONV #( iv_name ) ) TO lt_includes.
    ENDIF.

    DATA: lt_wbcrossgt TYPE wbcrossgtt.
    SELECT * FROM wbcrossgt INTO CORRESPONDING FIELDS OF TABLE @lt_wbcrossgt
      FOR ALL ENTRIES IN @lt_includes
      WHERE include = @lt_includes-table_line
      AND name <> @iv_name.
    IF lines( lt_wbcrossgt ) = 0.
* update so it is correct in the next run
      update_index( lv_clsname ).

      SELECT * FROM wbcrossgt INTO CORRESPONDING FIELDS OF TABLE @lt_wbcrossgt
        FOR ALL ENTRIES IN @lt_includes
        WHERE include = @lt_includes-table_line
        AND name <> @iv_name.
    ENDIF.

    IF iv_level < mv_max_level.
      resolve(
        EXPORTING
          it_wbcrossgt = lt_wbcrossgt
        CHANGING
          ct_tadir     = ct_tadir ).
    ELSE.
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.


  METHOD get_dependencies.

    DATA: lv_obj_type    TYPE euobj-id,
          lt_tadir       TYPE if_ris_environment_types=>ty_t_senvi_tadir,
          lt_environment TYPE senvi_tab.

*    IF is_object-obj_name = 'UCONHTTPSYSFIELDS_BASED_ON_SRV'.
*      BREAK-POINT.
*    ENDIF.

    IF iv_level > 1 AND is_object-object = 'CLAS'.
      find_clas_dependencies(
        EXPORTING
          iv_name  = is_object-obj_name
          iv_level = iv_level
        CHANGING
          ct_tadir = lt_tadir ).
    ELSEIF is_object-object = 'TABL'.
* do not traverse further
    ELSE.
      lv_obj_type = is_object-object.
      CALL FUNCTION 'REPOSITORY_ENVIRONMENT_SET'
        EXPORTING
          obj_type       = lv_obj_type
          object_name    = is_object-obj_name
        TABLES
          environment    = lt_environment
        EXCEPTIONS
          batch          = 1
          batchjob_error = 2
          not_executed   = 3
          OTHERS         = 4.
      IF sy-subrc = 3.
        RETURN.
      ELSEIF sy-subrc <> 0.
        BREAK-POINT.
      ENDIF.

      cl_wb_ris_environment=>convert_senvi_to_tadir(
        EXPORTING
          senvi       = lt_environment
        IMPORTING
          senvi_tadir = lt_tadir ).
    ENDIF.

    SORT lt_tadir BY ref_obj_type ASCENDING ref_obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_tadir COMPARING ref_obj_type ref_obj_name.

    DELETE lt_tadir WHERE ref_obj_type = 'FUGR'.
    DELETE lt_tadir WHERE ref_obj_type = 'DTEL'.
    DELETE lt_tadir WHERE ref_obj_type = 'SFSW'.
    DELETE lt_tadir WHERE ref_obj_type = 'DEVC'.
    DELETE lt_tadir WHERE ref_obj_type = 'SUSO'.
    DELETE lt_tadir WHERE ref_obj_type = 'TYPE'.
    DELETE lt_tadir WHERE ref_obj_type = 'TTYP'.
    DELETE lt_tadir WHERE ref_obj_type = 'PROG'.
    DELETE lt_tadir WHERE ref_obj_type = 'DOMA'.
    DELETE lt_tadir WHERE ref_obj_type = 'XSLT'.
    DELETE lt_tadir WHERE ref_obj_type = 'SHLP'.
    DELETE lt_tadir WHERE ref_obj_type = 'SQLT'.

    LOOP AT lt_tadir INTO DATA(ls_tadir).
      DATA(lv_index) = sy-tabix.
      SELECT SINGLE devclass FROM tadir INTO @DATA(lv_devclass)
        WHERE pgmid = 'R3TR'
        AND object = @ls_tadir-ref_obj_type
        AND obj_name = @ls_tadir-ref_obj_name.
* todo, should check if its a sub-package
      IF sy-subrc <> 0 OR lv_devclass CP |{ mv_package }*|.
        DELETE lt_tadir INDEX lv_index.
        CONTINUE.
      ENDIF.

      READ TABLE mt_total WITH KEY
        ref_obj_type = ls_tadir-ref_obj_type
        ref_obj_name = ls_tadir-ref_obj_name TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        DELETE lt_tadir INDEX lv_index.
      ENDIF.
    ENDLOOP.

    APPEND LINES OF lt_tadir TO mt_total.

    LOOP AT lt_tadir INTO ls_tadir.
      DATA(ls_object) = VALUE zif_abapgit_definitions=>ty_tadir(
        object = ls_tadir-ref_obj_type
        obj_name = ls_tadir-ref_obj_name ).

      DATA(lv_level) = iv_level + 1.

      get_dependencies(
        is_object = ls_object
        iv_level  = lv_level ).
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve.

    LOOP AT it_wbcrossgt INTO DATA(ls_wbcrossgt).
      CASE ls_wbcrossgt-otype.
        WHEN 'TY'.
          SELECT SINGLE clstype FROM seoclass INTO @DATA(lv_clstype) WHERE clsname = @ls_wbcrossgt-name(30).
          IF sy-subrc = 0.
            CASE lv_clstype.
              WHEN '0'.
                APPEND VALUE #( ref_obj_type = 'CLAS' ref_obj_name = ls_wbcrossgt-name ) TO ct_tadir.
              WHEN '1'.
                APPEND VALUE #( ref_obj_type = 'INTF' ref_obj_name = ls_wbcrossgt-name ) TO ct_tadir.
              WHEN OTHERS.
                ASSERT 0 = 1.
            ENDCASE.
          ENDIF.
        WHEN OTHERS.
          CONTINUE. " todo ?
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD update_index.

    DATA: lo_cross   TYPE REF TO cl_wb_crossreference,
          lv_include TYPE programm.

    lv_include = cl_oo_classname_service=>get_classpool_name( iv_name ).

    CREATE OBJECT lo_cross
      EXPORTING
        p_name    = lv_include
        p_include = lv_include.

    lo_cross->index_actualize( ).

  ENDMETHOD.
ENDCLASS.
