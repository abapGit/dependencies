CLASS zcl_abapgit_deps DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_git_url TYPE string
        !iv_package TYPE devclass .
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
    DATA mv_package TYPE devclass .

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

    rs_stage-comment-committer-email = 'upload@localhost'.
    rs_stage-comment-committer-name = 'upload'.
    rs_stage-comment-comment = 'Upload'.

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
    mv_package = iv_package.
    mv_branch  = 'refs/heads/master'.

  ENDMETHOD.


  METHOD get_local.

    DATA(lt_tadir) = NEW zcl_abapgit_deps_find( mv_package )->find( ).

    rt_local = NEW zcl_abapgit_deps_serializer( )->serialize( lt_tadir ).

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
