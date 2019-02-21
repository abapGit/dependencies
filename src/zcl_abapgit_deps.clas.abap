CLASS zcl_abapgit_deps DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor
      IMPORTING
        !iv_git_url TYPE string
        !iv_package TYPE devclass .
    METHODS run
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
        VALUE(rs_stage) TYPE ty_stage .
    METHODS get_local
      RETURNING
        VALUE(rt_local) TYPE zif_abapgit_definitions=>ty_files_tt
      RAISING
        zcx_abapgit_exception .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ABAPGIT_DEPS IMPLEMENTATION.


  METHOD build_stage.

* todo
    BREAK-POINT.

  ENDMETHOD.


  METHOD constructor.

    mv_git_url = iv_git_url.
    mv_package = iv_package.
    mv_branch  = 'master'.

  ENDMETHOD.


  METHOD get_local.

    DATA(lt_tadir) = NEW zcl_abapgit_deps_find( mv_package )->find( ).

    rt_local = NEW zcl_abapgit_deps_serializer( )->serialize( lt_tadir ).

  ENDMETHOD.


  METHOD run.

    DATA(lt_local) = get_local( ).

*    DATA(ls_remote) = zcl_abapgit_git_porcelain=>pull(
*      iv_url         = mv_git_url
*      iv_branch_name = mv_branch ).
*
*    DATA(ls_stage) = build_stage(
*      it_local  = lt_local
*      it_remote = ls_remote-files ).
*
*    zcl_abapgit_git_porcelain=>push(
*      is_comment     = ls_stage-comment
*      io_stage       = ls_stage-stage
*      it_old_objects = ls_remote-objects
*      iv_parent      = ls_remote-branch
*      iv_url         = mv_git_url
*      iv_branch_name = mv_branch ).

  ENDMETHOD.
ENDCLASS.
