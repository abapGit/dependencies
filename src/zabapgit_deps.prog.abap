REPORT zabapgit_deps.

PARAMETERS: p_git  TYPE text200 OBLIGATORY,
            p_devc TYPE devclass OBLIGATORY.

INCLUDE zabapgit_password_dialog.
INCLUDE zabapgit_forms.

INITIALIZATION.
  lcl_password_dialog=>on_screen_init( ).

AT SELECTION-SCREEN OUTPUT.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_output( ).
  ENDIF.

AT SELECTION-SCREEN.
  IF sy-dynnr = lcl_password_dialog=>c_dynnr.
    lcl_password_dialog=>on_screen_event( sscrfields-ucomm ).
  ENDIF.

START-OF-SELECTION.
  PERFORM deps.

FORM deps RAISING zcx_abapgit_exception.

  NEW zcl_abapgit_deps(
    iv_git_url = CONV #( p_git )
    iv_package = p_devc )->run( ).

ENDFORM.
