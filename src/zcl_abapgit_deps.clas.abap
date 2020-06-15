CLASS zcl_abapgit_deps DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_git_url     TYPE string
        !iv_packages    TYPE tab_packages
        !iv_git_name    TYPE string
        !iv_git_email   TYPE string
        !iv_git_comment TYPE string .
    METHODS run
      IMPORTING
        !iv_test TYPE abap_bool DEFAULT abap_false
      RAISING
        zcx_abapgit_exception .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_stage,
        comment TYPE zif_abapgit_definitions=>ty_comment,
        stage   TYPE REF TO zcl_abapgit_stage,
      END OF ty_stage .

    DATA mv_branch TYPE string .
    DATA mv_git_url TYPE string .
    DATA mv_packages TYPE tab_packages .
    DATA mv_git_name TYPE string .
    DATA mv_git_email TYPE string .
    DATA mv_git_comment TYPE string .

    METHODS build_stage
      IMPORTING
        !it_local       TYPE zif_abapgit_definitions=>ty_files_tt
        !it_remote      TYPE zif_abapgit_definitions=>ty_files_tt
      RETURNING
        VALUE(rs_stage) TYPE ty_stage
      RAISING
        zcx_abapgit_exception .
    METHODS get_local
      RETURNING
        VALUE(rt_local) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_DEPS IMPLEMENTATION.


  METHOD build_stage.

    rs_stage-comment-committer-email = mv_git_email.
    rs_stage-comment-committer-name = mv_git_name.
    rs_stage-comment-comment = mv_git_comment.

    rs_stage-stage = NEW #( ).

    LOOP AT it_local INTO DATA(ls_local).
      READ TABLE it_remote WITH KEY
        path = ls_local-path
        filename = ls_local-filename
        sha1 = ls_local-sha1 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        WRITE: / 'Add', ls_local-path, ls_local-filename.
        rs_stage-stage->add(
          iv_path     = ls_local-path
          iv_filename = ls_local-filename
          iv_data     = ls_local-data ).
      ENDIF.
    ENDLOOP.

    LOOP AT it_remote INTO DATA(ls_remote).
      READ TABLE it_local WITH KEY
        path = ls_remote-path
        filename = ls_remote-filename TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        WRITE: / 'Remove', ls_local-path, ls_local-filename.
        rs_stage-stage->rm(
          iv_path     = ls_remote-path
          iv_filename = ls_remote-filename ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mv_git_url = iv_git_url.
    mv_packages = iv_packages.
    mv_branch  = 'refs/heads/master'.
    mv_git_name = iv_git_name.
    mv_git_email = iv_git_email.
    mv_git_comment = iv_git_comment.

  ENDMETHOD.


  METHOD get_local.

    DATA: lp_package LIKE LINE OF mv_packages.

    LOOP AT mv_packages INTO lp_package.
      DATA(lt_tadir) = NEW zcl_abapgit_deps_find( lp_package )->find( ).
      DATA(lt_local) = NEW zcl_abapgit_deps_serializer( )->serialize( lt_tadir ).
      APPEND LINES OF lt_local TO rt_local.
    ENDLOOP.

  ENDMETHOD.


  METHOD run.

    DATA(lt_local) = get_local( ).

    DATA(ls_remote) = zcl_abapgit_git_porcelain=>pull(
      iv_url         = mv_git_url
      iv_branch_name = mv_branch ).

* dont push to repositories containing abapgit code
* everything will be overwritten in the remote repo
    READ TABLE ls_remote-files WITH KEY path = '/' filename = '.abapgit.xml'
      TRANSPORTING NO FIELDS.
    ASSERT sy-subrc <> 0.

    DELETE ls_remote-files WHERE path <> '/src/'.

    DATA(ls_stage) = build_stage(
      it_local  = lt_local
      it_remote = ls_remote-files ).

    IF ls_stage-stage->count( ) > 0 AND iv_test = abap_false.
      zcl_abapgit_git_porcelain=>push(
        is_comment     = ls_stage-comment
        io_stage       = ls_stage-stage
        it_old_objects = ls_remote-objects
        iv_parent      = ls_remote-branch
        iv_url         = mv_git_url
        iv_branch_name = mv_branch ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
